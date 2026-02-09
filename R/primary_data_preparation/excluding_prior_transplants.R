# Let's exclude patients from the merged dataset who have had a prior transplant. 
# If a patient has multiple SLK transplants, we will only include the first event. 
# Special cases (same day re-transplant) were removed manually.
# Only first SLK transplant was included


# ---- Exclude patients with any prior transplant (of any organ) ----
tx_slk_preclean <- tx_slkRaw %>%
  filter(
    (is.na(CAN_PREV_TX_LI) | CAN_PREV_TX_LI == 0) &
    (is.na(CAN_PREV_TX_KI) | CAN_PREV_TX_KI == 0)
  )

# ---- Prior liver/kidney transplant ----
#now using transplant date, look for those who have prior liver/kidney transplant by date compared to slk date  
# Combine just the ID, organ, and date info
all_tx <- bind_rows(
  tx_li %>% select(PERS_ID, ORG_TY = REC_TX_ORG_TY, REC_TX_DT),
  tx_ki %>% select(PERS_ID, ORG_TY = REC_TX_ORG_TY, REC_TX_DT)
)

# ----Earliest transplant date per patient----
first_tx <- all_tx %>%
  group_by(PERS_ID) %>%
  summarise(first_tx_date = min(REC_TX_DT), .groups = "drop")

# ----Find SLK date----
slk_dates <- tx_slk_preclean %>%
  mutate(SLK_DATE = pmin(REC_TX_DT_LI, REC_TX_DT_KI, na.rm = TRUE)) %>%
  group_by(PERS_ID) %>%
  summarise(slk_date = min(SLK_DATE), .groups = "drop")

# ----Merge and identify those with prior liver/kidney transplants----
slk_with_prior <- slk_dates %>%
  left_join(first_tx, by = "PERS_ID") %>%
  mutate(has_prior_tx = first_tx_date < slk_date)

# ----Keep only first-time SLK patients----
tx_slk_clean <- tx_slk_preclean %>%
  semi_join(
    slk_with_prior %>% filter(!has_prior_tx),
    by = "PERS_ID"
  )

# ----if patient has >1 SLK transplant, keep earliest date----
# Keep only the earliest SLK event per patient, but retain same-day duplicates 
tx_slk_first <- tx_slk_clean %>%
  group_by(PERS_ID) %>%
  mutate(first_slk_date = min(pmin(REC_TX_DT_LI, REC_TX_DT_KI), na.rm = TRUE)) %>% 
  filter(pmin(REC_TX_DT_LI, REC_TX_DT_KI) == first_slk_date) %>%
  ungroup() %>%
  select(-first_slk_date)

# ----Overall dimensions----
dim(tx_slk_first) #10214

# ----Number of unique patients----
n_distinct(tx_slk_first$PERS_ID) #10212, 

# ----which patients are duplicated----
dupes <- tx_slk_first %>%
  count(PERS_ID) %>%
  filter(n > 1) %>%
  arrange(desc(n))

nrow(dupes)   # 2


# ----Diagnostic: types of duplicates,Count how many SLK rows per patient----
dupe_summary <- tx_slk_first %>%
  mutate(SLK_DATE = pmin(REC_TX_DT_LI, REC_TX_DT_KI, na.rm = TRUE)) %>%
  group_by(PERS_ID) %>%
  summarise(
    n_rows = n(),
    n_unique_dates = n_distinct(SLK_DATE),
    earliest_slk = min(SLK_DATE),
    latest_slk = max(SLK_DATE),
    .groups = "drop"
  ) %>%
  mutate(
    duplicate_type = case_when(
      n_rows == 1 ~ "Single SLK",
      n_unique_dates == 1 ~ "Same-day duplicates",
      n_unique_dates > 1 ~ "Multiple SLK dates"
    )
  )

# ----View summary counts----
dupe_summary %>%
  count(duplicate_type)


# ----diagnostic check - how many duplicates remain after keeping earliest SLK? #2 same day duplicates----
tx_slk_first %>%
  count(PERS_ID) %>%
  filter(n > 1) %>%
  nrow()

# ----Identify same-day duplicates ----
same_day_dupes <- dupe_summary %>%
  filter(duplicate_type == "Same-day duplicates") %>%
  select(PERS_ID)

# ----Pull the corresponding rows from full dataset----
same_day_dupe_rows <- tx_slk_clean %>%
  semi_join(same_day_dupes, by = "PERS_ID") %>%
  arrange(PERS_ID, REC_TX_DT_LI, REC_TX_DT_KI)

# ----Write to CSV for manual inspection----
write_csv(
  same_day_dupe_rows %>%
    select(PERS_ID, REC_TX_DT_LI, REC_TX_DT_KI, REC_FAIL_DT_LI, REC_FAIL_DT_KI,everything()),
  "data-private/same_day_duplicates_for_review.csv"
)

# ----manually identified duplicates, now creating a DF with exact rows to remove ----
rows_to_remove <- tribble(
  ~PERS_ID, ~REC_FAIL_DT_LI, ~REC_FAIL_DT_KI,
  2117076, as.Date("1997-12-19"), as.Date("1997-08-14"),
  4505523, NA, NA
)

# ----Remove those rows from main dataset----
tx_slk_clean <- tx_slk_first %>%
  anti_join(rows_to_remove, by = c("PERS_ID", "REC_FAIL_DT_LI", "REC_FAIL_DT_KI"))

# ----check to make sure that the correct rows were removed ----
tx_slk_first %>%
  filter(PERS_ID %in% rows_to_remove$PERS_ID)
tx_slk_clean %>%
  filter(PERS_ID %in% rows_to_remove$PERS_ID)

# ----check if each row of data set is unique persID
n_distinct(tx_slk_clean$PERS_ID)
nrow(tx_slk_clean)




# ----Now perform diagnostic checks of tx_slk_clean----


# ----Confirm each patient only appears once----
nrow(tx_slk_clean)
n_distinct(tx_slk_clean$PERS_ID)

# ----Check distribution of date differences between liver & kidney tx----
tx_slk_clean %>%
  mutate(date_diff = as.numeric(REC_TX_DT_LI - REC_TX_DT_KI)) %>%
  summarise(
    min_diff = min(date_diff),
    max_diff = max(date_diff),
    mean_diff = mean(date_diff)
  )

# ----Verify SLK definition: all should have both LI and KI dates present----
sum(is.na(tx_slk_clean$REC_TX_DT_LI))
sum(is.na(tx_slk_clean$REC_TX_DT_KI))

