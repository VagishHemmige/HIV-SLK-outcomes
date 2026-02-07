
#This section is for checking variables:
#raw data vs after conversion in srtr package

tx_li_raw <- tx_li_raw%>%
  select(REC_AGE_IN_MONTHS_AT_TX,REC_AGE_AT_TX, everything())

tx_slk_final<-tx_slk_final %>% 
  select(REC_AGE_IN_MONTHS_AT_TX_LI,REC_AGE_AT_TX_LI, everything())

table(tx_slk_final$CAN_LAST_SRTR_LAB_MELD)
summary(tx_li_raw$CAN_LAST_SRTR_LAB_MELD)


#MELD vs PELD after extracting numeric data only 
summary(tx_slk_final$CAN_LAST_SRTR_LAB_MELD_NUM)
table(tx_slk_final$CAN_LAST_SRTR_LAB_MELD_NUM, useNA = "ifany")

tx_slk_final %>% 
  filter(is.na(CAN_LAST_SRTR_LAB_MELD_NUM) & !is.na(CAN_LAST_SRTR_LAB_MELD)) %>% 
  select(CAN_LAST_SRTR_LAB_MELD) %>% 
  distinct() %>% 
  head(30)
tx_slk_final %>%
  filter(CAN_LAST_SRTR_LAB_MELD_NUM < 0) %>%
  select(CAN_LAST_SRTR_LAB_MELD, CAN_LAST_SRTR_LAB_MELD_TY,REC_AGE_IN_MONTHS_AT_TX_LI, REC_AGE_AT_TX) %>%
  distinct()

tx_slk_final %>%
  filter(CAN_LAST_SRTR_LAB_MELD_NUM < 0) %>%
  View()


#Let's do some diagnostic checks of final data set
# Dimensions and first few rows
dim(tx_slk)
head(tx_slk)

# Missing values in key columns
tx_slk %>%
  summarise(
    missing_pers_id = sum(is.na(PERS_ID)),
    missing_date = sum(is.na(REC_TX_DT)),
    missing_org_ty = sum(is.na(ORG_TY)),
    missing_race = sum(is.na(DON_RACE)),
    missing_org_ty= sum(is.na(REC_TX_ORG_TY)),
    missing_graft_fail_date= sum(is.na(REC_FAIL_DT)) #11472????
  )

#frequency counts, check join results
# How many per patient?
tx_slk %>%
  count(PERS_ID) %>%
  summarise(
    one_tx = sum(n == 1),
    two_tx = sum(n == 2),
    more = sum(n > 2)
  )

# Count by organ type
tx_slk %>%
  count(ORG_TY)

# Cross-tab by organ type and year
tx_slk %>%
  mutate(tx_year = lubridate::year(REC_TX_DT)) %>%
  count(tx_year, ORG_TY) %>%
  tidyr::pivot_wider(names_from = ORG_TY, values_from = n, values_fill = 0)
#time difference between kidney and liver
tx_slk %>%
  group_by(PERS_ID) %>%
  summarise(
    date_diff = as.numeric(max(REC_TX_DT) - min(REC_TX_DT)),
    .groups = "drop"
  ) %>%
  count(date_diff) #there are a lot of dates>1 day which doesn't make sense

tx_li %>% count(PERS_ID) %>% filter(n > 1) #12903
tx_ki %>% count(PERS_ID) %>% filter(n > 1) #45210
tx_slkRaw %>% filter(abs(as.numeric(REC_TX_DT_LI - REC_TX_DT_KI)) <= 1)


#Now, check counts to see if merge makes sense 
n_distinct(tx_slk$PERS_ID)
nrow(tx_slk) ##difference in counts doesn't make sense, must be duplicate PERS_IDs?

tx_slk <- tx_slk %>%
  select(REC_TX_DT, ORG_TY, everything())

tx_slk %>%
  count(PERS_ID, REC_TX_DT) %>%
  filter(n > 1)

tx_slkRaw %>% 
  filter(PERS_ID == "3905643") %>% 
  select(PERS_ID, REC_TX_DT_LI, REC_TX_DT_KI, everything()) %>% 
  arrange(REC_TX_DT_LI, REC_TX_DT_KI)

```



Identify mismatched SLK donors and export to excel
```{r}
mismatch_rows <- which(tx_slk_final$DONOR_ID_LI != tx_slk_final$DONOR_ID_KI)
mismatch_data <- tx_slk_final[mismatch_rows, ]

desired_front <- c(
  "PERS_ID",
  "DONOR_ID_LI", "DONOR_ID_KI",
  "DON_TY_LI","DON_TY_KI",
  "DON_AGE_LI", "DON_AGE_KI",
  "DON_RACE_LI", "DON_RACE_KI",
  "DON_DEATH_MECH_LI", "DON_DEATH_MECH_KI"
)

reordered_data <- mismatch_data %>%
  select(all_of(desired_front), everything())

write_xlsx(reordered_data, "SLK_Donor_Mismatches_2.xlsx")
table(mismatch_data$HIV_POSITIVE)

sum(mismatch_data$REC_HIV_STAT_KI == "P", na.rm = TRUE)



#finding discordant HIV cases
find_P_discordance <- function(df, root, id_var = "PERS_ID") {
  
  li_name <- paste0(root, "_LI")
  ki_name <- paste0(root, "_KI")
  
  if (!all(c(li_name, ki_name) %in% names(df))) {
    stop(paste("Columns", li_name, "and/or", ki_name, "not found in data frame"))
  }
  
  df %>%
    mutate(
      li_val = .data[[li_name]],
      ki_val = .data[[ki_name]]
    ) %>%
    filter(
      # one is P, the other is NOT P
      (li_val == "P" & ki_val != "P") |
        (ki_val == "P" & li_val != "P")
    ) %>%
    select(
      !!sym(id_var),
      REC_TX_DT_LI, REC_TX_DT_KI,
      DONOR_ID_LI, DONOR_ID_KI,
      !!sym(li_name), !!sym(ki_name)
    )
}

discordant_hiv_P <- find_P_discordance(tx_slk_final, "REC_HIV_STAT")
discordant_hiv_P



roots_to_check <- c(
  "REC_HIV_STAT",
  "REC_HCV_STAT",
  "REC_HBV_ANTIBODY",
  "REC_HBV_SURF_ANTIGEN",
  "CAN_MALIG"
)

discordant_list <- lapply(roots_to_check, function(root) {
  out <- find_P_discordance(tx_slk_final, root)
  if (nrow(out) > 0) out$variable <- root
  out
})

discordant_P_all <- bind_rows(discordant_list)
discordant_P_all







find_discordant_li_ki <- function(df, root, id_var = "PERS_ID") {
  li_name <- paste0(root, "_LI")
  ki_name <- paste0(root, "_KI")
  
  if (!all(c(li_name, ki_name) %in% names(df))) {
    stop(paste("Columns", li_name, "and/or", ki_name, "not found in data frame"))
  }
  
  df %>%
    mutate(
      li_val = .data[[li_name]],
      ki_val = .data[[ki_name]]
    ) %>%
    # keep rows where BOTH are non-missing and DIFFERENT
    filter(
      !is.na(li_val),
      !is.na(ki_val),
      li_val != ki_val
    ) %>%
    select(
      !!sym(id_var),
      REC_TX_DT_LI, REC_TX_DT_KI,
      DONOR_ID_LI, DONOR_ID_KI,
      !!sym(li_name),
      !!sym(ki_name)
    )
}


roots_to_check <- c(
  "REC_HIV_STAT",
  "REC_HCV_STAT",
  "REC_HBV_ANTIBODY",
  "REC_HBV_SURF_ANTIGEN",
  "CAN_MALIG"     # your malignancy flag
)


discordant_list <- lapply(roots_to_check, function(root) {
  res <- find_discordant_li_ki(tx_slk_final, root = root, id_var = "PERS_ID")
  if (nrow(res) > 0) {
    res$variable_root <- root
  }
  res
})

# bind all discordant rows into one data frame
discordant_all <- bind_rows(discordant_list)

# look at it
discordant_all


discordant_hiv <- find_discordant_li_ki(tx_slk_final, root = "REC_HIV_STAT", id_var = "PERS_ID")
discordant_hiv


discordant_counts <- discordant_all %>%
  count(variable_root)

discordant_counts

