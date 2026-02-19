

# Now I will create a data set that contains liver-only transplants (excluding SLK patients), excludes pediatric patients, 
# excludes patients with prior transplant, and removes duplicates. using tx_slk_final data set 

# ----starting with all liver transplants, exclude SLK PERSIDs----
tx_li_only<-tx_li %>% 
  filter(!PERS_ID %in% tx_slk_final$PERS_ID)

# ----Exclude patients with prior transplants using CAN_PREV_TX----
tx_li_only_preclean <- tx_li_only %>%
  filter(
    (is.na(CAN_PREV_TX) | CAN_PREV_TX == 0)
  )

# ----Add first transplant info to exclude prior tx by date----
tx_li_only_clean <- tx_li_only_preclean %>%
  left_join(first_tx, by = "PERS_ID") %>%
  filter(first_tx_date == REC_TX_DT)  # keeps only first transplant per patient

# ----Date verification----
# Ensure no overlap between SLK and liver-only datasets
intersect_ids <- intersect(tx_slk_final$PERS_ID, tx_li_only_clean$PERS_ID)
length(intersect_ids)  # should be 0

#Check counts
cat("Liver-only rows:", nrow(tx_li_only_clean), "\n")
cat("Unique patients:", n_distinct(tx_li_only_clean$PERS_ID), "\n")

# ----Remove duplicate patients, keeping only first liver transplant----
tx_li_only_clean %>%
  count(PERS_ID) %>%
  filter(n > 1) %>%
  arrange(desc(n))

#---- Export pts to identify duplicates manually----
# Identify patients with more than one liver transplant
li_dupes <- tx_li_only_clean %>%
  count(PERS_ID) %>%
  filter(n > 1)

# Pull full rows for these duplicate patients
li_dupe_rows <- tx_li_only_clean %>%
  semi_join(li_dupes, by = "PERS_ID") %>%
  arrange(PERS_ID, REC_TX_DT)

# Add an empty column to flag patients for removal later
li_dupe_rows <- li_dupe_rows %>%
  mutate(REMOVE_FLAG = NA_character_)  # you’ll fill this manually in Excel

#Export to working directory for manual review
write_csv(
  li_dupe_rows %>%
    select(PERS_ID, REC_TX_DT, REC_FAIL_DT, REC_PX_STAT, REC_FAIL_REJ_ACUTE, REMOVE_FLAG, everything()),  # make important columns visible first
  "data-private/liver_duplicate_patients_for_review_new.csv"
)


if (FALSE){
# ----Read manually edited file----
manual_flags <- read_csv("data-private/liver_duplicate_patients_for_review_new(in).csv")

# ----Filter only the rows flagged for removal and keep exact PERS_ID + REC_PX_STAT----
rows_to_remove <- manual_flags %>%
  filter(tolower(REMOVE_FLAG) == "remove") %>%
  select(PERS_ID, REC_PX_STAT) %>%
  distinct()  # ensures no duplicates

# ----Remove these flagged rows from main dataset----
tx_li_only_clean2 <- tx_li_only_clean %>%
  anti_join(rows_to_remove, by = c("PERS_ID", "REC_PX_STAT"))

# ----Check the result----
cat("Final liver-only rows:", nrow(tx_li_only_clean2), "\n")
cat("Unique patients:", n_distinct(tx_li_only_clean2$PERS_ID), "\n")


# ----Let's process Liver only data set----

# Create recipient age variable and exclude pediatrics patients <16----
tx_li_only_final<-tx_li_only_clean2 %>%
   mutate(
     REC_AGE_YEARS = REC_AGE_IN_MONTHS_AT_TX / 12
   ) %>%  
  filter(REC_AGE_YEARS>=16) %>% 
##Create numerical HIV_POSITIVE variable (assume missing is negative)
  mutate(HIV_POSITIVE=ifelse(REC_HIV_STAT=="P", 1, 0)) %>% 
#Calculate Liver donor BMI
  mutate(DON_BMI=DON_WGT_KG/(DON_HGT_CM/100)^2) %>% 
  #Turn liver failure date into a failure/censor date, with a separate variable to 
  #determine which of the two applies
  mutate(TFL_GRAFT_DT_LI_censor=if_else(is.na(TFL_GRAFT_DT), 
                                   TFL_LAFUDATE,
                                   TFL_GRAFT_DT))%>%
  mutate(TFL_GRAFT_DT_LI_binary=ifelse(is.na(TFL_GRAFT_DT),0,1))%>%
  mutate(TFL_GRAFT_DT_LI_yrs = (1/365)+
      as.numeric(
        difftime(TFL_GRAFT_DT_LI_censor, 
                 REC_TX_DT, 
                 units = "days")) / 365.25) %>% 
    #Create a composite death variable
  mutate(REC_DEATH_DT_COMPOSITE_LI=case_when(
    !is.na(PERS_OPTN_DEATH_DT)~PERS_OPTN_DEATH_DT,
    is.na(PERS_OPTN_DEATH_DT)&!is.na(PERS_SSA_DEATH_DT)~PERS_SSA_DEATH_DT,
    is.na(PERS_OPTN_DEATH_DT)&is.na(PERS_SSA_DEATH_DT)~TFL_DEATH_DT
  ))%>%
  #Create a time to death variable
  mutate(REC_DEATH_DT_CENSOR_LI=if_else(is.na(REC_DEATH_DT_COMPOSITE_LI), 
                                   TFL_LAFUDATE,
                                   REC_DEATH_DT_COMPOSITE_LI))%>%
  mutate(REC_DEATH_BINARY_LI=ifelse(is.na(REC_DEATH_DT_COMPOSITE_LI),0,1))%>%
  mutate(REC_DEATH_yrs_LI = 
      as.numeric(
        difftime(REC_DEATH_DT_CENSOR_LI, 
                 REC_TX_DT, 
                 units = "days")) / 365.25) %>% 
#Create median time on waitlist until transplant variable
  mutate(median_waitlist_time_years_li=as.numeric(difftime(REC_TX_DT,
                                       CAN_LISTING_DT,
                                     units="days"))/365.25) %>%
#create year of transplant variable 
  mutate(YEAR_TRANSPLANT=year(REC_TX_DT)) %>% 
#create numeric MELD value
  mutate(
    CAN_LAST_SRTR_LAB_MELD_NUM = str_extract(
      CAN_LAST_SRTR_LAB_MELD,
      regex("(?<=MELD/PELD)\\s*-?\\d+", ignore_case = TRUE)
    ),
    CAN_LAST_SRTR_LAB_MELD_NUM = as.integer(CAN_LAST_SRTR_LAB_MELD_NUM)
  ) %>% 
#create separate MELD and PELD number
  mutate(MELD_NUM_Last = ifelse(CAN_LAST_SRTR_LAB_MELD_TY == "M", 
                             CAN_LAST_SRTR_LAB_MELD_NUM, 
                             NA)) %>%
#Create numeric PELD variable (only where type is "P")
  mutate(PELD_NUM_Last = ifelse(CAN_LAST_SRTR_LAB_MELD_TY == "P", 
                           CAN_LAST_SRTR_LAB_MELD_NUM, 
                           NA)) %>% 
#create numeric candidate age at listing variable 
  mutate(
  CAN_AGE_AT_LISTING_YR_LI = CAN_AGE_IN_MONTHS_AT_LISTING/12
    )%>% 
#make variable names same as in SLK (also add in any that will be in table)
  rename(
    DON_AGE_LI    = DON_AGE,
    DON_GENDER_LI = DON_GENDER,
    DON_BMI_LI    = DON_BMI,
    DON_DEATH_MECH_LI = DON_DEATH_MECH
  )



}