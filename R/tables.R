#Tables

#Let's perform univariate analysis of final SLK data set, HIV slk vs not-HIV SLK, SLK HIV vs Liver only HIV

#make lists of variables of interest
Don_Var_List<-c("DON_AGE",
                "DON_GENDER",
                "DON_ETHNICITY_SRTR",
                "DON_RACE",
                "DON_EXPAND_DON_KI",
                "DON_DEATH_MECH_CAT",
                "DON_HIV_NAT_RECODED",
                "DON_ANTI_HIV_recode",
                "DON_HCV_STAT_RECODED",
                "DON_ANTI_HCV_RECODED",
                "DON_HBC_STAT_RECODED"
                )

Rec_Li_Var_List<-c("REC_AGE_YEARS",
                   "REC_AGE_AT_TX",
                   "REC_COLD_ISCH_TM_LI",
                   "TRANSPLANT_REASON_LI",
                   "REC_FAIL_HEP_DENOVO_recode",
                   "REC_FAIL_HEP_RECUR_recode",
                   "REC_FAIL_VASC_THROMB_recode",
                   "REC_FAIL_INFECT_LI_recode",
                   "REC_FAIL_RECUR_DISEASE_LI_recode",
                   "REC_FAIL_REJ_ACUTE_LI_recode",
                   "REC_ACUTE_REJ_EPISODE_LI",
                   "TFL_GRAFT_DT_LI_yrs",
                   "REC_DEATH_yrs"
                  )
Rec_Ki_Var_List<-c("REC_ACUTE_REJ_EPISODE_KI",
                   "REC_COLD_ISCH_TM_KI",
                   "TRANSPLANT_REASON_KI",
                   "TRANSPLANT_REASON_KI_COLLAPSED",
                   "REC_FAIL_GRAFT_THROMB_KI",
                   "REC_FAIL_INFECT_KI_recode",
                   "REC_FAIL_RECUR_DISEASE_KI_recode",
                   "REC_FAIL_REJ_ACUTE_KI_recode",
                   "REC_FAIL_SURG_COMPL_recode",
                   "REC_FAIL_UROL_COMPL_recode",
                   "REC_HBV_ANTIBODY_COMB",
                   "REC_HBV_SURF_ANTIGEN_COMB",
                   "REC_HCV_STAT_COMB",
                   "REC_CMV_COMBINED",
                   "REC_MALIG_TY_HEPCARCINOMA_COMB",
                   "REC_MALIG_TY_LIVER_COMB",
                   "TFL_GRAFT_DT_KI_yrs"
                   )
Cand_Li_Var_List<-c("CAN_PORTAL_VEIN_recode",
                    "CAN_TIPSS_recode",
                    "CAN_ASCITES_recode",
                    "CAN_BACTERIA_PERIT_recode",
                    "CAN_ENCEPH_recode",
                    "CAN_LAST_ASCITES",
                    "CAN_LAST_ALBUMIN",
                    "CAN_LAST_BILI",
                    "CAN_LAST_INR",
                    "CAN_LAST_SERUM_CREAT",
                    "CAN_LAST_SERUM_SODIUM",
                    "CAN_LAST_ENCEPH",
                    "MELD_NUM_Init",
                    "MELD_NUM_Last",
                    "DON_TY",
                    "median_waitlist_time_years_li"
                    )
Cand_Ki_Var_List<-c("CAN_AGE_AT_LISTING_YR_KI",
                    "CAN_GENDER",
                    "CAN_ETHNICITY_SRTR",
                    "CAN_RACE",
                    "CAN_RACE_collapsed",
                    "CAN_BMI_clean",
                    "CAN_CREAT_CLEAR",
                    "CAN_DIAB",
                    "CAN_DIAL",
                    "TIME_ON_DIALYSIS_YEARS",
                    "CAN_DRUG_TREAT_HYPERTEN_COMB",
                    "CAN_FUNCTN_STAT_clean",
                    "CAN_GFR",
                    "CAN_MALIG_COMB",
                    "median_waitlist_time_years_ki"
)


#create donor characteristics table 
don_table <-
  tbl_summary(
    tx_slk_final,
    include = all_of(Don_Var_List),
    by = HIV_POSITIVE,
    missing = "ifany",
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    label = list(
      DON_DEATH_MECH_CAT ~ "Donor Death Mechanism",
      DON_AGE ~ "Donor Age",
      DON_GENDER ~ "Donor Gender",
      DON_ETHNICITY_SRTR ~ "Donor Ethnicity",
      DON_RACE ~ "Donor Race",
      DON_EXPAND_DON_KI ~ "Meets Expanded Kidney Donor Criteria",
      DON_HIV_NAT_RECODED ~ "Donor HIV RNA NAT Serology",
      DON_HCV_STAT_RECODED ~ "Donor HCV Status",
      DON_ANTI_HCV_RECODED ~ "Donor Anti-HCV Status",
      DON_HBC_STAT_RECODED ~ "Donor HBC Status",
      DON_ANTI_HIV_recode ~ "Donor Anti-HIV Status"
    )
  ) %>%
  add_p(
    test = all_categorical() ~ "fisher.test",
    test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**Recipient HIV Status**") %>%
  bold_labels() %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        label = dplyr::case_when(
          
          # F/M recode for donor gender
          variable == "DON_GENDER" & label == "F" ~ "Female",
          variable == "DON_GENDER" & label == "M" ~ "Male",
          
          # everything else unchanged
          TRUE ~ label
        )
      )
  ) %>%
  as_gt() %>%
  gt::cols_label(
    stat_1 = "Negative/Unknown",
    stat_2 = "Positive"
  ) %>%
  gt::tab_header(
    title = "SLK Donor Characteristics"
  ) %>%
  gt::gtsave("don_table.png")


#create recipient table for liver variables 
rec_li_table <- tbl_summary(
    data = tx_slk_final,
    include = all_of(Rec_Li_Var_List),
    by = HIV_POSITIVE,     
    missing = "ifany",
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    label = list(REC_AGE_YEARS ~ "Recipient Age (Years)", REC_AGE_AT_TX ~ "Recipient Age at Transplant", TRANSPLANT_REASON_LI ~ "Primary Liver Diagnosis", REC_FAIL_HEP_DENOVO_recode~"Cause of Graft Failure: Hepatitis: Denovo", REC_FAIL_VASC_THROMB_recode~"	Cause of Graft Failure: Vascular Thrombosis", REC_FAIL_INFECT_LI_recode="Cause of Graft Failure: Infection", REC_FAIL_REJ_ACUTE_LI_recode~"Cause of Graft Failure: Acute Rejection", REC_FAIL_HEP_RECUR_recode~ "Cause of Graft Failure: Recurrent Hepatitis", REC_ACUTE_REJ_EPISODE_LI~"Acute Liver Rejection Episode between Transplant and Discharge", TFL_GRAFT_DT_LI_yrs~"Time to Graft Failure (Years)", REC_DEATH_yrs~"Follow-up time (Years)", REC_FAIL_RECUR_DISEASE_LI_recode~"Cause of Graft Failure: Recurrent Disease" )
  ) %>%
  add_p(
    test = all_categorical() ~ "fisher.test",
    test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**HIV Status**") %>% 
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        label = dplyr::case_when(
          label == "Y" ~ "Yes",
          label == "N" ~ "No",
          label == "U" ~ "Unknown",
          TRUE ~ label
        )
      )
  ) %>% 
  bold_labels() %>%   
  as_gt() %>%
  gt::cols_label(
    stat_1 = "Negative/Unknown",
    stat_2 = "Positive"
  ) %>%
  gt::tab_header(
    title = "SLK Recipient Characteristics - Liver Variables"
  ) %>% 
  gt::gtsave("rec_li_table.png")

##create recipient table for kidney variables 
rec_ki_table<- tbl_summary(
    data = tx_slk_final,
    include = all_of(Rec_Ki_Var_List),
    by = HIV_POSITIVE,     
    missing = "ifany",
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    label = list(REC_ACUTE_REJ_EPISODE_KI~"Acute Kidney Rejection Episode between Transplant and Discharge", TRANSPLANT_REASON_KI ~ "Primary Kidney Diagnosis", REC_FAIL_GRAFT_THROMB_KI~"	Cause of Graft Failure: Vascular Thrombosis", REC_FAIL_INFECT_KI_recode="Cause of Graft Failure: Infection", REC_FAIL_RECUR_DISEASE_KI_recode~"Cause of Graft Failure: Recurrent Disease", REC_FAIL_REJ_ACUTE_KI_recode~"Cause of Graft Failure: Acute Rejection", REC_FAIL_SURG_COMPL_recode~"Cause of Graft Failure:Surgical Complications", REC_FAIL_UROL_COMPL_recode~"Cause of Graft Failure: Urological Complications", TFL_GRAFT_DT_KI_yrs~"Time to Graft Failure (Years)", REC_HBV_ANTIBODY_COMB~"Recipient HBV Core Antibody",REC_HBV_SURF_ANTIGEN_COMB~"Recipient HBV Surface Antigen", REC_HCV_STAT_COMB~"Recipient HCV Status", REC_MALIG_TY_HEPCARCINOMA_COMB~"Previous Malignancy - Hepatocellular Carcinoma", REC_MALIG_TY_LIVER_COMB~"Previous Malignancy - Liver")
  ) %>%
  add_p(
    test = all_categorical() ~ "fisher.test",
    test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**HIV Status**") %>% 
  bold_labels() %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
      label = dplyr::case_when(
        variable %in% c(
          "REC_FAIL_GRAFT_THROMB_recode",
          "REC_FAIL_INFECT_KI_recode",
          "REC_FAIL_RECUR_DISEASE_KI_recode",
          "REC_FAIL_REJ_ACUTE_KI_recode",
          "REC_FAIL_SURG_COMPL_recode",
          "REC_FAIL_UROL_COMPL_recode",
          "REC_FAIL_GRAFT_THROMB_KI",
          "REC_MALIG_TY_LIVER_COMB",
          "REC_MALIG_TY_HEPCARCINOMA_COMB"
        ) & label == "Y" ~ "Yes",
        variable %in% c(
          "REC_FAIL_GRAFT_THROMB_recode",
          "REC_FAIL_INFECT_KI_recode",
          "REC_FAIL_RECUR_DISEASE_KI_recode",
          "REC_FAIL_REJ_ACUTE_KI_recode",
          "REC_FAIL_SURG_COMPL_recode",
          "REC_FAIL_UROL_COMPL_recode",
          "REC_FAIL_GRAFT_THROMB_KI",
          "REC_MALIG_TY_LIVER_COMB",
          "REC_MALIG_TY_HEPCARCINOMA_COMB"
        ) & label == "N" ~ "No",
        variable %in% c(
          "REC_FAIL_GRAFT_THROMB_recode",
          "REC_FAIL_INFECT_KI_recode",
          "REC_FAIL_RECUR_DISEASE_KI_recode",
          "REC_FAIL_REJ_ACUTE_KI_recode",
          "REC_FAIL_SURG_COMPL_recode",
          "REC_FAIL_UROL_COMPL_recode",
          "REC_FAIL_GRAFT_THROMB_KI",
          "REC_MALIG_TY_LIVER_COMB",
          "REC_MALIG_TY_HEPCARCINOMA_COMB"
        ) & label == "U" ~ "Unknown",

        #Variables using P / N / U => Positive / Negative / Unknown
        variable %in% c(
          "REC_HBV_ANTIBODY_COMB",
          "REC_HBV_SURF_ANTIGEN_COMB",
          "REC_HCV_STAT_COMB",
          "REC_CMV_COMBINED"
        ) & label == "P" ~ "Positive",
        variable %in% c(
          "REC_HBV_ANTIBODY_COMB",
          "REC_HBV_SURF_ANTIGEN_COMB",
          "REC_HCV_STAT_COMB",
          "REC_CMV_COMBINED"
        ) & label == "N" ~ "Negative",
        variable %in% c(
          "REC_HBV_ANTIBODY_COMB",
          "REC_HBV_SURF_ANTIGEN_COMB",
          "REC_HCV_STAT_COMB",
          "REC_CMV_COMBINED"
        ) & label == "U" ~ "Unknown",
        TRUE ~ label
      )
    )
) %>% 
  as_gt() %>%
  gt::cols_label(
    stat_1 = "Negative/Unknown",
    stat_2 = "Positive"
  ) %>%
  gt::tab_header(
    title = "SLK Recipient Characteristics - Kidney Variables"
  ) %>% 
  gt::gtsave("rec_ki_table.png")
  
#create table for candidate liver variables 
cand_li_table <- tbl_summary(
    data = tx_slk_final,
    include = all_of(Cand_Li_Var_List),
    by = HIV_POSITIVE,     
    missing = "ifany",
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    label = list(
      MELD_NUM_Init ~ "Initial MELD Score",
      MELD_NUM_Last ~ "Final MELD Score",
      CAN_LAST_ASCITES ~ "Candidate Last Ascites",
      CAN_LAST_ENCEPH ~ "Candidate Last Encephalopathy",
      median_waitlist_time_years_li ~ "Median Waitlist Time (years)"
  )) %>%
  add_p(
    test = all_categorical() ~ "fisher.test",
    test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**HIV Status**") %>% 
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        label = dplyr::case_when(
          label == "Y" ~ "Yes",
          label == "N" ~ "No",
          label == "U" ~ "Unknown",
          TRUE ~ label
        )
      )
  ) %>% 
  bold_labels() %>%   
  as_gt() %>%
  gt::cols_label(
    stat_1 = "Negative/Unknown",
    stat_2 = "Positive"
  ) %>%
  gt::tab_header(
    title = "SLK Candidate Characteristics - Liver Variables"
  ) %>% 
  gt::gtsave("cand_li_table.png")


#create table for candidate kidney variables 
cand_kipa_table <- tbl_summary(
    data = tx_slk_final,
    include = all_of(Cand_Ki_Var_List),
    by = HIV_POSITIVE,     
    missing = "ifany",
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 2,
    label = list(CAN_DIAL ~ "Dialysis", 
                 CAN_DIAB ~ "Diabetes", 
                 CAN_RACE ~ "Candidate Race", 
                 CAN_ETHNICITY_SRTR~"Candidate Ethnicity", 
                 median_waitlist_time_years_ki~"Median Waitlist Time (Years)", 
                 CAN_FUNCTN_STAT_clean~"Candidate Functional Status", 
                 CAN_AGE_AT_LISTING_YR_KI~ "Candidate Age at Listing (Years)", 
                 CAN_GENDER ~ "Candidate Gender",
                 TIME_ON_DIALYSIS_YEARS ~ "Time on Dialysis Prior to Transplant (Years)",
                 CAN_BMI_clean ~ "Candidate BMI (kg/m^2)")) %>%
  add_p(
    test = all_categorical() ~ "fisher.test",
    test.args = all_categorical() ~ list(simulate.p.value = TRUE, B = 10000)
  ) %>%
  modify_spanning_header(c(stat_1, stat_2) ~ "**HIV Status**") %>% 
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        label = dplyr::case_when(
          label == "Y" ~ "Yes",
          label == "N" ~ "No",
          label == "U" ~ "Unknown",
          TRUE ~ label
        )
      )
  ) %>% 
  bold_labels() %>%
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        label = dplyr::case_when(
          # F/M recode for donor gender
          variable == "CAN_GENDER" & label == "F" ~ "Female",
          variable == "CAN_GENDER" & label == "M" ~ "Male",
          
          # everything else unchanged
          TRUE ~ label
        )
      )
  ) %>% 
  as_gt() %>%
  gt::cols_label(
    stat_1 = "Negative/Unknown",
    stat_2 = "Positive"
  ) %>%
  gt::tab_header(
    title = "SLK Candidate Characteristics - Kidney Variables"
  ) %>% 
  gt::gtsave("cand_ki_table.png")






##below from pancreas code for example
Matching_Variable_List<-c("PDRI",
                          "TRANSPLANT_REASON",
                          "REC_AGE_AT_TX",
                          "CAN_GENDER",
                          "REC_RACE_FACTOR")


Demographic_Variable_List<-c("DON_BMI",
                             "DON_WGT_KG",
                             "DON_HGT_CM",
                             "DON_GENDER",
                             "DON_AGE_IN_MONTHS",
                             "DON_CREAT",
                             "DON_RACE",
                             "DON_RACE_FACTOR",
                             "DON_COD_DON_STROKE",
                             "REC_PA_PRESERV_TM",
                             "REC_PREV_KI",
                             "DON_HIST_DIAB",
                             "REC_DUCT_FACTOR",
                             "REC_WGT_KG",
                             "REC_BMI",
                             "CAN_LAST_ALLOC_PRA",
                             "CAN_LAST_CUR_PRA",
                             "CAN_LAST_SRTR_PEAK_PRA",
                             "DON_ANTI_HCV",
                             "REC_CMV_STAT",
                             "REC_CMV_IGG",
                             "REC_HCV_STAT",
                             "DON_ANTI_CMV",
                             "Center_volume_factor_pancreas",
                             "Center_volume_factor_kidney",
                             "Total_center_volume_pancreas",
                             "Total_center_volume_kidney",
                             "Center_HIV_volume_kidney",
                             "center_HIV_volume_kidney_factor",
                             "median_waitlist_time_years"
                             )

Outcome_Variable_List<-c("REC_POSTX_LOS",
                         "TFL_GRAFT_DT_KI", 
                         "TFL_GRAFT_DT_KI_censor",
                         "TFL_GRAFT_DT_KI_binary",
                         "TFL_GRAFT_DT_KI_yrs",
                         "TFL_GRAFT_DT_PA", 
                         "TFL_GRAFT_DT_PA_censor",
                         "TFL_GRAFT_DT_PA_binary",
                         "TFL_GRAFT_DT_PA_yrs",
                         "REC_ACUTE_REJ_EPISODE_KI",
                         "REC_ACUTE_REJ_EPISODE_PA",
                         "REC_ANAST_LEAK",
                         "REC_ABSCESS",
                         "REC_PANCREATITIS",
                         "REC_REJ_DT_KI_CENSOR",
                         "REC_REJ_KI_BINARY",
                         "FIRST_FU_REJ_DATE_KI",
                         "REC_REJ_TIME_KI_yrs",
                         "REC_REJ_DT_PA_CENSOR",
                         "REC_REJ_PA_BINARY",
                         "FIRST_FU_REJ_DATE_PA",
                         "REC_REJ_TIME_PA_yrs",
                         "PERS_OPTN_DEATH_DT",
                         "REC_DEATH_DT_COMPOSITE",
                         "REC_DEATH_DT_CENSOR",
                         "REC_DEATH_BINARY",
                         "REC_DEATH_yrs",
                         "REC_COD",
                         "REC_COD2",
                         "REC_COD3",
                         "REC_MALIG_DT_CENSOR", 
                         "REC_MALIG_BINARY", 
                         "REC_MALIG_TIME_yrs")

```

Manuscript Matching
```{r Manuscript Matching}
#prepare matching data set
match_df <- tx_slk_final %>%
  filter(YEAR_TRANSPLANT >= 2005) %>%
  select(
    PERS_ID,
    HIV_POSITIVE,
    YEAR_TRANSPLANT,
    REC_TX_DT,
    ERA,

    # recipient
    REC_AGE_YEARS,
    CAN_GENDER,
    CAN_RACE,
    CAN_RACE_collapsed,
    MELD_NUM_Last,
    TRANSPLANT_REASON_LI,
    TRANSPLANT_REASON_KI,
    TRANSPLANT_REASON_KI_COLLAPSED,
    REC_HCV_STAT_COMB,
    REC_CMV_COMBINED,
    REC_HBV_SURF_ANTIGEN_COMB,
    REC_HBV_ANTIBODY_COMB,
    REC_DEATH_yrs,
    REC_DEATH_BINARY,
    TFL_GRAFT_DT_KI,
    TFL_GRAFT_DT_KI_binary,
    TFL_GRAFT_DT_KI_yrs,
    TFL_GRAFT_DT_KI_censor,
    TFL_GRAFT_DT_LI,
    TFL_GRAFT_DT_LI_binary,
    TFL_GRAFT_DT_LI_yrs,
    TFL_GRAFT_DT_LI_censor,
    los_days,
    los_days_inclusive,
    
    # donor
    DON_AGE,
    DON_GENDER,
    DONOR_ID
  ) %>%
  mutate(
    YEAR_TRANSPLANT = factor(YEAR_TRANSPLANT),
    CAN_GENDER = factor(CAN_GENDER),
    DON_GENDER = factor(DON_GENDER),
    CAN_RACE_collapsed   = factor(CAN_RACE_collapsed),
    TRANSPLANT_REASON_LI = factor(TRANSPLANT_REASON_LI),
    TRANSPLANT_REASON_KI = factor(TRANSPLANT_REASON_KI),
    TRANSPLANT_REASON_KI_COLLAPSED = factor(TRANSPLANT_REASON_KI_COLLAPSED),
    REC_HCV_STAT_COMB = factor(REC_HCV_STAT_COMB)
    
  )



#convert transplant date to numeric and check
match_df$REC_TX_DT <- as.Date(match_df$REC_TX_DT)
match_df$REC_TX_DT_num <- as.numeric(match_df$REC_TX_DT)
summary(match_df$REC_TX_DT)
summary(match_df$REC_TX_DT_num)


#turn NAs into "Missing"
# After: match_df <- readRDS("...")

# Helper: convert NA / "" / " " to "Missing" and return a factor
make_missing_level <- function(x, missing_label = "Missing") {
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA
  x[is.na(x)] <- missing_label
  factor(x)
}

# Apply to the covariates MatchIt complained about
match_df$REC_HCV_STAT_COMB <- make_missing_level(match_df$REC_HCV_STAT_COMB, "Missing")
match_df$REC_CMV_COMBINED <- make_missing_level(match_df$REC_CMV_COMBINED, "Missing")
match_df$REC_HBV_SURF_ANTIGEN_COMB <- make_missing_level(match_df$REC_HBV_SURF_ANTIGEN_COMB, "Missing")
match_df$REC_HBV_ANTIBODY_COMB <- make_missing_level(match_df$REC_HBV_ANTIBODY_COMB, "Missing")

#save dataset for matching, export to baseR 
# Save updated dataset for base R matching
saveRDS(match_df, "~/Desktop/slk_match/match_df_for_baseR_cmv_hbv.rds")

#Base R matching formula
# > ps_formula <- HIV_POSITIVE ~
# +   REC_AGE_YEARS +
# +   CAN_GENDER +
# +   CAN_RACE_collapsed +
# +   MELD_NUM_Last +
# +   REC_HCV_STAT_COMB +
# +   TRANSPLANT_REASON_LI +
# +   TRANSPLANT_REASON_KI_COLLAPSED +
# +   DON_AGE +
# +   DON_GENDER +
# +   REC_HBV_SURF_ANTIGEN_COMB +
# +   REC_CMV_COMBINED
# # > 
# > m_full_cmv_hbv <- matchit(
# +   formula     = ps_formula,
# +   data        = match_df,
# +   method      = "nearest",
# +   ratio       = 5,
# +   exact       = ~ ERA,                   # <-- exact match on era
# +   caliper     = c(REC_TX_DT_num = 365),  # <-- 365-day caliper on tx date
# +   std.caliper = FALSE
# + )


#export match back into Rstudio
matched_obj <- readRDS("~/Desktop/slk_match/matched_cmv_hbv_era_exact_txcal365.rds")

dim(matched_obj)
table(matched_obj$HIV_POSITIVE)
summary(matched_obj$weights)
"subclass" %in% names(matched_obj)   # should be TRUE

#build matched dataset

# Keep only the matching metadata + your join key
match_meta <- matched_obj %>%
  dplyr::select(PERS_ID, subclass, weights, distance)

# Pull all analysis variables from tx_slk_final for the matched people
matched_df <- tx_slk_final %>%
  dplyr::semi_join(match_meta, by = "PERS_ID") %>%   # keeps only matched people
  dplyr::left_join(match_meta, by = "PERS_ID")       # adds subclass/weights/distance back

# Sanity checks
stopifnot(nrow(matched_df) == nrow(matched_obj))     # should match
stopifnot(!any(is.na(matched_df$subclass)))          # subclass should exist for all
table(matched_df$HIV_POSITIVE)


var_labels <- c(

  # Donor variables
  DON_AGE = "Donor age (years)",
  DON_GENDER = "Donor sex",
  DON_ETHNICITY_SRTR = "Donor ethnicity",
  DON_RACE = "Donor race",
  DON_EXPAND_DON_KI = "Expanded criteria donor (kidney)",
  DON_DEATH_MECH_CAT = "Donor mechanism of death",
  DON_HIV_NAT_RECODED = "Donor HIV NAT",
  DON_ANTI_HIV_recode = "Donor anti-HIV antibody",
  DON_HCV_STAT_RECODED = "Donor HCV status",
  DON_ANTI_HCV_RECODED = "Donor anti-HCV antibody",
  DON_HBC_STAT_RECODED = "Donor hepatitis B core antibody",

  # Recipient – Liver
  REC_AGE_YEARS = "Recipient age (years)",
  REC_AGE_AT_TX = "Recipient age at transplant (years)",
  REC_COLD_ISCH_TM_LI = "Liver cold ischemia time (hours)",
  TRANSPLANT_REASON_LI = "Primary liver diagnosis",
  REC_FAIL_HEP_DENOVO_recode = "De novo hepatitis after transplant",
  REC_FAIL_HEP_RECUR_recode = "Recurrent hepatitis after transplant",
  REC_FAIL_VASC_THROMB_recode = "Liver graft failure due to vascular thrombosis",
  REC_FAIL_INFECT_LI_recode = "Liver graft failure due to infection",
  REC_FAIL_RECUR_DISEASE_LI_recode = "Liver graft failure due to recurrent disease",
  REC_FAIL_REJ_ACUTE_LI_recode = "Liver graft failure due to acute rejection",
  REC_ACUTE_REJ_EPISODE_LI = "Acute liver rejection before discharge",
  TFL_GRAFT_DT_LI_yrs = "Time to liver graft failure (years)",
  REC_DEATH_yrs = "Time to death (years)",

  # Recipient – Kidney
  REC_ACUTE_REJ_EPISODE_KI = "Acute kidney rejection before discharge",
  REC_COLD_ISCH_TM_KI = "Kidney cold ischemia time (hours)",
  TRANSPLANT_REASON_KI = "Primary kidney diagnosis",
  REC_FAIL_GRAFT_THROMB_KI = "Kidney graft failure due to thrombosis",
  REC_FAIL_INFECT_KI_recode = "Kidney graft failure due to infection",
  REC_FAIL_RECUR_DISEASE_KI_recode = "Kidney graft failure due to recurrent disease",
  REC_FAIL_REJ_ACUTE_KI_recode = "Kidney graft failure due to acute rejection",
  REC_FAIL_SURG_COMPL_recode = "Kidney graft failure due to surgical complication",
  REC_FAIL_UROL_COMPL_recode = "Kidney graft failure due to urologic complication",
  REC_HBV_ANTIBODY_COMB = "Recipient hepatitis B antibody",
  REC_HBV_SURF_ANTIGEN_COMB = "Recipient hepatitis B surface antigen",
  REC_HCV_STAT_COMB = "Recipient hepatitis C status",
  REC_CMV_COMBINED = "Recipient CMV status",
  REC_MALIG_TY_HEPCARCINOMA_COMB = "History of hepatocellular carcinoma",
  REC_MALIG_TY_LIVER_COMB = "History of other liver malignancy",
  TFL_GRAFT_DT_KI_yrs = "Time to kidney graft failure (years)",

  # Candidate – Liver
  CAN_PORTAL_VEIN_recode = "Portal vein thrombosis",
  CAN_TIPSS_recode = "Prior TIPS",
  CAN_ASCITES_recode = "Ascites at listing",
  CAN_BACTERIA_PERIT_recode = "History of bacterial peritonitis",
  CAN_ENCEPH_recode = "Hepatic encephalopathy at listing",
  CAN_LAST_ASCITES = "Ascites at transplant",
  CAN_LAST_ALBUMIN = "Serum albumin at transplant (g/dL)",
  CAN_LAST_BILI = "Total bilirubin at transplant (mg/dL)",
  CAN_LAST_INR = "INR at transplant",
  CAN_LAST_SERUM_CREAT = "Serum creatinine at transplant (mg/dL)",
  CAN_LAST_SERUM_SODIUM = "Serum sodium at transplant (mEq/L)",
  CAN_LAST_ENCEPH = "Encephalopathy at transplant",
  MELD_NUM_Init = "Initial MELD score",
  MELD_NUM_Last = "MELD score at transplant",
  DON_TY = "Donor type",
  median_waitlist_time_years_li = "Median liver waitlist time (years)",

  # Candidate – Kidney
  CAN_AGE_AT_LISTING_YR_KI = "Age at kidney listing (years)",
  CAN_GENDER = "Recipient sex",
  CAN_ETHNICITY_SRTR = "Recipient ethnicity",
  CAN_RACE = "Recipient race",
  CAN_RACE_collapsed = "Recipient race collapsed",
  CAN_BMI_clean = "Body mass index (kg/m²)",
  CAN_CREAT_CLEAR = "Creatinine clearance",
  CAN_DIAB = "Diabetes mellitus",
  CAN_DIAL = "On dialysis at listing",
  TIME_ON_DIALYSIS_YEARS = "Time on dialysis (years)",
  CAN_DRUG_TREAT_HYPERTEN_COMB = "Treated hypertension",
  CAN_FUNCTN_STAT_clean = "Functional status",
  CAN_GFR = "Glomerular filtration rate",
  CAN_MALIG_COMB = "History of malignancy",
  median_waitlist_time_years_ki = "Median kidney waitlist time (years)"
)


#fix variable levels
recode_YNU <- function(x) {
  recode(x,
         "Y" = "Yes",
         "N" = "No",
         "U" = "Unknown",
         .default = as.character(x))
}

recode_PNU <- function(x) {
  recode(x,
         "P" = "Positive",
         "N" = "Negative",
         "U" = "Unknown",
         .default = as.character(x))
}



matched_df <- matched_df %>%
  set_variable_labels(
    DON_AGE = "Donor age, years",
    DON_GENDER = "Donor sex",
    DON_RACE = "Donor race",
    DON_HIV_NAT_RECODED = "Donor HIV status (NAT)",
    DON_ANTI_HIV_recode = "Donor anti-HIV antibody",
    CAN_BMI_clean = "Recipient BMI",
    CAN_GENDER = "Recipient sex",
    CAN_RACE_collapsed = "Recipient race",
    REC_AGE_YEARS = "Recipient age, years",
    TRANSPLANT_REASON_LI = "Primary Liver Diagnosis",
    TRANSPLANT_REASON_KI_COLLAPSED = "Primary Kidney Diagnosis",
    MELD_NUM_Last = "MELD score at transplant",
    TIME_ON_DIALYSIS_YEARS = "Time on dialysis, years"
  )

matched_df <- matched_df %>%
  mutate(
    REC_HBV_ANTIBODY_COMB = recode_PNU(REC_HBV_ANTIBODY_COMB),
    REC_HBV_SURF_ANTIGEN_COMB = recode_PNU(REC_HBV_SURF_ANTIGEN_COMB),
    REC_HCV_STAT_COMB = recode_PNU(REC_HCV_STAT_COMB),
    REC_CMV_COMBINED = recode_PNU(REC_CMV_COMBINED)
  )

make_gtsummary_labels <- function(named_vec) {
  lapply(names(named_vec), function(v) {
    as.formula(paste0(v, " ~ ", shQuote(unname(named_vec[[v]]))))
  })
}

label_list <- make_gtsummary_labels(var_labels)


tbl_dat <- matched_df %>%
  dplyr::select(
    HIV_POSITIVE,
    all_of(Don_Var_List),
    all_of(Rec_Li_Var_List),
    all_of(Rec_Ki_Var_List),
    all_of(Cand_Li_Var_List),
    all_of(Cand_Ki_Var_List)
  )

# "self-healing" labels: only keep labels for vars that exist in tbl_dat
label_list_ok <- label_list[names(label_list) %in% names(tbl_dat)]


tab1 <-
  tbl_dat %>%
  gtsummary::tbl_summary(
    by = HIV_POSITIVE,
    missing = "ifany",
    statistic = list(
      gtsummary::all_continuous() ~ "{median} ({p25}, {p75})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    label = label_list_ok
  ) %>%
  gtsummary::add_p(
    test = gtsummary::all_categorical() ~ "fisher.test",
    test.args = gtsummary::all_categorical() ~ list(simulate.p.value = TRUE)
  ) %>%
  as_gt() %>%
  gt::cols_label(
    stat_1 = "Negative/Unknown",
    stat_2 = "Positive"
  ) %>%
  gt::tab_header(
    title = "Characteristics of HIV-positive and HIV-negative SLK recipients (matched cohort)"
  )

gt::gtsave(tab1, "tables/matched_table_df_full.png")


# Making condensed table for poster


matched_df <- matched_df %>%
  mutate(
    # Sex labels
    DON_GENDER = recode_sex_FM(DON_GENDER),
    CAN_GENDER = recode_sex_FM(CAN_GENDER),
    
    # Donor race collapsed to match recipient race buckets
    DON_RACE_collapsed = collapse_donor_race(DON_RACE),
  ) %>%
  mutate(
    # nice ordering for tables
    DON_GENDER = factor(DON_GENDER, levels = c("Female", "Male")),
    CAN_GENDER = factor(CAN_GENDER, levels = c("Female", "Male")),
    
    DON_RACE_collapsed = factor(DON_RACE_collapsed,
                                levels = c("White", "Black", "Hispanic/Latino", "Other", "Missing"))
  ) %>%
  set_variable_labels(
    DON_GENDER = "Donor sex",
    DON_RACE_collapsed = "Donor race",
    DON_ANTI_HIV_recode = "Donor anti-HIV antibody",
    
    CAN_GENDER = "Recipient sex",
    CAN_RACE_collapsed = "Recipient race",
    
    REC_HBV_ANTIBODY_COMB = "HBV core antibody",
    REC_HBV_SURF_ANTIGEN_COMB = "HBV surface antigen",
    REC_HCV_STAT_COMB = "HCV serology",
    REC_CMV_COMBINED = "CMV serology"
  )


matched_df <- matched_df %>%
  mutate(
    # ---- serologies: Unknown -> NA ----
    REC_HBV_ANTIBODY_COMB     = na_if(REC_HBV_ANTIBODY_COMB, "Unknown"),
    REC_HBV_SURF_ANTIGEN_COMB = na_if(REC_HBV_SURF_ANTIGEN_COMB, "Unknown"),
    REC_HCV_STAT_COMB         = na_if(REC_HCV_STAT_COMB, "Unknown"),
    REC_CMV_COMBINED          = na_if(REC_CMV_COMBINED, "Unknown"),
    
    # ---- donor anti-HIV: Unknown -> NA ----
    DON_ANTI_HIV_recode = na_if(DON_ANTI_HIV_recode, "Unknown"),
    
    # ---- drop unused factor levels ----
    across(where(is.factor), fct_drop)
  )


matched_df <- matched_df %>%
  mutate(
    DON_RACE_collapsed = case_when(
      DON_RACE %in% c("8", "White") ~ "White",
      DON_RACE %in% c("16", "Black or African American") ~ "Black",
      DON_RACE %in% c("2000", "Hispanic/Latino") ~ "Hispanic/Latino",
      is.na(DON_RACE) ~ NA_character_,
      TRUE ~ "Other"
    ),
    DON_RACE_collapsed = factor(
      DON_RACE_collapsed,
      levels = c("White", "Black", "Hispanic/Latino", "Other")
    )
  )


short_var_list<-c("DON_AGE",
                  "DON_GENDER",
                  "CAN_BMI_clean",
                  "REC_AGE_YEARS",
                  "CAN_GENDER",
                  "CAN_RACE_collapsed",
                  "TRANSPLANT_REASON_LI",
                  "TRANSPLANT_REASON_KI_COLLAPSED",
                  "REC_HBV_SURF_ANTIGEN_COMB",
                  "REC_HCV_STAT_COMB",
                  "REC_CMV_COMBINED",
                  "MELD_NUM_Last"
)



#condensed table
tbl_dat2 <- matched_df %>%
  dplyr::select(
    HIV_POSITIVE,
    all_of(short_var_list)
  )

# "self-healing" labels: only keep labels for vars that exist in tbl_dat
label_list_ok <- label_list[names(label_list) %in% names(tbl_dat)]


tab2 <-
  tbl_dat2 %>%
  gtsummary::tbl_summary(
    by = HIV_POSITIVE,
    missing = "ifany",
    statistic = list(
      gtsummary::all_continuous() ~ "{median} ({p25}, {p75})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    label = label_list_ok
  ) %>%
  gtsummary::add_p(
    test = gtsummary::all_categorical() ~ "fisher.test",
    test.args = gtsummary::all_categorical() ~ list(simulate.p.value = TRUE)
  ) %>%
  as_gt() %>%
  gt::cols_label(
    stat_1 = "Negative/Unknown",
    stat_2 = "Positive"
  ) %>%
  gt::tab_header(
    title = "Characteristics of HIV-positive and HIV-negative SLK recipients (matched cohort)"
  )

gt::gtsave(tab2, "tables/matched_table_condensed_2.png")




#make tables
# Kidney graft failure events
with(matched_df, table(KI_EVENT, useNA = "ifany"))
# Liver graft failure events
with(matched_df, table(LI_EVENT, useNA = "ifany"))

# By era (optional)
with(matched_df, table(HIV_POSITIVE, KI_EVENT))
with(matched_df, table(HIV_POSITIVE, LI_EVENT))


#make kidney failure tables
#clean labels
t_kid_unadj <- clean_labels(mk_tbl(cox_kidney))
t_kid_era   <- clean_labels(mk_tbl(cox_kidney_era))
t_kid_int   <- clean_labels(mk_tbl(cox_kidney_int))
t_kid_pre   <- clean_labels(mk_tbl(cox_kidney_pre))
t_kid_post  <- clean_labels(mk_tbl(cox_kidney_post))

#create table
tbl_kidney_gf <-
  tbl_merge(
    tbls = list(
      t_kid_unadj,
      t_kid_era,
      t_kid_int,
      t_kid_pre,
      t_kid_post
    ),
    tab_spanner = c(
      "**Unadjusted**",
      "**Adjusted for ERA**",
      "**HIV × ERA**",
      "**Pre-DAA only**",
      "**Post-DAA only**"
    )
  ) %>%
  modify_caption("**Table 2. Cox proportional hazards models for kidney graft failure (matched cohort)**") %>%
  modify_footnote(
    everything() ~ "Robust standard errors clustered by matched set (subclass)."
  )

tbl_kidney_gf

#change table orientation
tbl_kidney_gf_long <- tbl_stack(
  tbls = list(t_kid_unadj, t_kid_era, t_kid_int, t_kid_pre, t_kid_post),
  group_header = c(
    "Unadjusted",
    "Adjusted for ERA",
    "HIV × ERA interaction",
    "Pre-DAA only",
    "Post-DAA only"
  )
) %>%
  modify_caption("**Table 2. Cox proportional hazards models for kidney graft failure (matched cohort)**") %>%
  modify_footnote(everything() ~ "Robust standard errors clustered by matched set (subclass).")

tbl_kidney_gf_long


#make liver tables
t_liv_unadj <- clean_labels(mk_tbl(cox_liver))
t_liv_era   <- clean_labels(mk_tbl(cox_liver_era))
t_liv_pre   <- clean_labels(mk_tbl(cox_liver_pre))
t_liv_post  <- clean_labels(mk_tbl(cox_liver_post))

tbl_liver_gf <-
  tbl_merge(
    tbls = list(
      t_liv_unadj,
      t_liv_era,
      t_liv_pre,
      t_liv_post
    ),
    tab_spanner = c(
      "**Unadjusted**",
      "**Adjusted for ERA**",
      "**Pre-DAA only**",
      "**Post-DAA only**"
    )
  ) %>%
  modify_caption("**Table 3. Cox proportional hazards models for liver graft failure (matched cohort)**") %>%
  modify_footnote(
    everything() ~
      "Robust standard errors clustered by matched set (subclass). Post-DAA estimates may be unstable due to sparse events."
  )

tbl_liver_gf

#change table orientation
tbl_liver_gf_long <- tbl_stack(
  tbls = list(t_liv_unadj, t_liv_era, t_liv_pre, t_liv_post),
  group_header = c(
    "Unadjusted",
    "Adjusted for ERA",
    "Pre-DAA only",
    "Post-DAA only"
  )
) %>%
  modify_caption("**Table 3. Cox proportional hazards models for liver graft failure (matched cohort)**") %>%
  modify_footnote(
    everything() ~ "Robust standard errors clustered by matched set (subclass). Post-DAA estimates may be unstable due to sparse events."
  )

tbl_liver_gf_long

```

table for poster
```{r making tables for poster}
library(gtsummary)
library(survival)
library(gt)

# -----------------------------
# 0) Helper functions
# -----------------------------
mk_tbl <- function(fit) {
  tbl_regression(
    fit,
    exponentiate = TRUE,
    estimate_fun = ~ style_sigfig(.x, digits = 2),   # keeps model output tidy
    pvalue_fun   = ~ style_pvalue(.x, digits = 2)
  ) %>%
    bold_labels()
}

clean_labels <- function(tbl) {
  tbl %>%
    modify_table_body(~ .x %>%
                        mutate(
                          label = case_when(
                            label == "HIV_POSITIVE" ~ "HIV-positive (vs HIV-negative)",
                            TRUE ~ label
                          )
                        )
    )
}

# Create HR (95% CI) with 2 decimals and keep p-value
shrink_one <- function(tbl) {
  tbl %>%
    modify_table_body(~ .x %>%
                        # keep only the HIV row
                        filter(term == "HIV_POSITIVE") %>%
                        mutate(
                          est_num  = suppressWarnings(as.numeric(estimate)),
                          low_num  = suppressWarnings(as.numeric(conf.low)),
                          high_num = suppressWarnings(as.numeric(conf.high)),
                          
                          hr_ci = case_when(
                            !is.na(est_num) ~ sprintf("%.2f (%.2f–%.2f)", est_num, low_num, high_num),
                            TRUE ~ NA_character_
                          ),
                          
                          p_fmt = case_when(
                            is.na(p.value) ~ NA_character_,
                            p.value < 0.01 ~ "<0.01",
                            TRUE ~ sprintf("%.2f", p.value)
                          )
                        ) %>%
                        select(label, hr_ci, p_fmt)
    ) %>%
    modify_header(
      hr_ci ~ "**HR (95% CI)**",
      p_fmt ~ "**p**"
    )
}


# Mark Post-DAA liver graft failure as not estimable (NE)
mark_ne <- function(tbl) {
  tbl %>%
    modify_table_body(~ .x %>%
                        mutate(
                          hr_ci = "NE",
                          p_fmt = NA_character_
                        )
    )
}

# -----------------------------
# 1) Build SMALL tables for each model (overall / pre / post) per outcome
# -----------------------------

# ---- Mortality ----
mort_overall <- clean_labels(mk_tbl(cox_main))       %>% shrink_one()
mort_pre     <- clean_labels(mk_tbl(cox_pre_unadj))  %>% shrink_one()
mort_post    <- clean_labels(mk_tbl(cox_post_unadj)) %>% shrink_one()

tbl_mort_block <- tbl_merge(
  tbls = list(mort_overall, mort_pre, mort_post),
  tab_spanner = c("**Overall**", "**Pre-DAA**", "**Post-DAA**")
)

# ---- Kidney graft failure ----
kid_overall <- clean_labels(mk_tbl(cox_kidney))      %>% shrink_one()
kid_pre     <- clean_labels(mk_tbl(cox_kidney_pre))  %>% shrink_one()
kid_post    <- clean_labels(mk_tbl(cox_kidney_post)) %>% shrink_one()

tbl_kid_block <- tbl_merge(
  tbls = list(kid_overall, kid_pre, kid_post),
  tab_spanner = c("**Overall**", "**Pre-DAA**", "**Post-DAA**")
)

# ---- Liver graft failure ----
liv_overall <- clean_labels(mk_tbl(cox_liver))      %>% shrink_one()
liv_pre     <- clean_labels(mk_tbl(cox_liver_pre))  %>% shrink_one()

# Post-DAA liver: quasi separation -> NE
liv_post <- clean_labels(mk_tbl(cox_liver_post)) %>%
  shrink_one() %>%
  mark_ne()

tbl_liv_block <- tbl_merge(
  tbls = list(liv_overall, liv_pre, liv_post),
  tab_spanner = c("**Overall**", "**Pre-DAA**", "**Post-DAA**")
)

# -----------------------------
# 2) Stack into the MEGA table + caption/footnote
# -----------------------------
tbl_mega <- tbl_stack(
  tbls = list(tbl_mort_block, tbl_kid_block, tbl_liv_block),
  group_header = c(
    "Overall mortality",
    "Kidney graft failure (28 events; 6 HIV+)",
    "Liver graft failure (16 events; 2 HIV+)"
  )
) %>%
  modify_caption("**Cox regression results (matched cohort), overall and stratified by DAA era**") %>%
  modify_footnote(
    everything() ~ paste(
      "Robust SEs clustered by matched set (subclass).",
      "Kidney graft failure: 28 total events (6 HIV+).",
      "Liver graft failure: 16 total events (2 HIV+).",
      "Post-DAA liver graft failure: NE = not estimable due to sparse/zero due to sparse/zero events (quasi separation)."
    )
  )

tbl_mega

# -----------------------------
# 3) Save as PNG for poster
# -----------------------------
gt_tbl <- tbl_mega %>%
  as_gt() %>%
  tab_options(
    table.width = pct(100),
    table.font.size = 11,
    data_row.padding = px(3)
  )

gtsave(gt_tbl, filename = "tables/cox_mega_table_poster.png")



interaction_df <- bind_rows(
  extract_interaction(cox_mort_int,   "Overall mortality"),
  extract_interaction(cox_kidney_int, "Kidney graft failure (28 events; 6 HIV+)"),
  extract_interaction(cox_liver_int,  "Liver graft failure (16 events; 2 HIV+)")
)


mega_df_final <- mega_df %>%
  left_join(interaction_df, by = "Outcome")

gt_tbl <- mega_df_final %>%
  gt(rowname_col = "Outcome") %>%
  tab_header(
    title = md("**Cox regression results (matched cohort; N=336 total, 56 HIV-positive)**"),
    subtitle = md("Overall, stratified by DAA era, and HIV × era interaction")
  ) %>%
  tab_source_note(
    md("Post-DAA liver graft failure: NE = not estimable due to sparse/zero due to sparse/zero events (quasi separation). Interaction ratio is the ratio of post-DAA to pre-DAA hazard ratios")
  ) %>%
  tab_options(
    table.width = pct(80),
    table.font.size = 11,
    data_row.padding = px(3)
  )

gtsave(gt_tbl, "cox_mega_table_with_interaction.png")







# ---- build LONG table ----
los_long_tbl <- dplyr::bind_rows(
  extract_robust(
    nb_los,
    cluster = los_df$subclass,
    model_label = "Negative binomial (unadjusted)",
    exponentiate = TRUE,
    keep_terms = c("HIV_POSITIVE")
  ),
  extract_robust(
    nb_los_era,
    cluster = los_df$subclass,
    model_label = "Negative binomial (+ ERA)",
    exponentiate = TRUE,
    keep_terms = c("HIV_POSITIVE", "ERAPost-DAA")
  ),
  extract_robust(
    lm_los,
    cluster = los_df$subclass,
    model_label = "Log-linear sensitivity (log LOS, + ERA)",
    exponentiate = TRUE,
    keep_terms = c("HIV_POSITIVE", "ERAPost-DAA")
  )
)

# ---- render as grouped long table ----
los_gt <- los_long_tbl %>%
  gt(groupname_col = "Model") %>%
  tab_header(title = "Length of hospital stay (discharged alive): regression results") %>%
  cols_label(
    Predictor = "Predictor",
    `Effect (95% CI)` = "Ratio (95% CI)",
    `Approx. % diff`  = "Approx. % difference",
    `p-value` = "p-value"
  ) %>%
  tab_source_note("Effect estimates are ratios (exp(beta)); robust SEs clustered by matched set (subclass). Outcome: los_days_inclusive.")

los_gt




# =========================
# 2) Create a single clean table (HR (95% CI) + p)
# =========================
tab_pre  <- format_cox(cox_pre,  "Pre-DAA (adjusted)")
tab_post <- format_cox(cox_post, "Post-DAA (adjusted)")
tab_int  <- format_cox(cox_int_adj, "All eras (HIV×ERA + adjusted)")

all_tabs <- bind_rows(tab_pre, tab_post, tab_int)

print(all_tabs)

# =========================
# 3) Export to a text file (easy to email)
# =========================
out_dir <- "~/Desktop/slk_match"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

txt_file <- file.path(out_dir, "SLK_Cox_PrePost_Interaction_Summary.txt")

sink(txt_file)
cat("SLK Cox Models (Matched Cohort)\n")
cat("=========================================\n\n")

cat("NOTE: Models use robust SEs clustered by matched set (subclass).\n")
cat("      Matching weights are all 1 (nearest neighbor without replacement).\n\n")

if (length(res_pre$warnings)) {
  cat("Warnings (Pre-DAA model):\n")
  cat(paste0("- ", unique(res_pre$warnings), collapse = "\n"), "\n\n")
}
if (length(res_post$warnings)) {
  cat("Warnings (Post-DAA model):\n")
  cat(paste0("- ", unique(res_post$warnings), collapse = "\n"), "\n\n")
}
if (length(res_int$warnings)) {
  cat("Warnings (Interaction model):\n")
  cat(paste0("- ", unique(res_int$warnings), collapse = "\n"), "\n\n")
}

cat("Table: Hazard ratios (95% CI) and p-values\n\n")
print(all_tabs, row.names = FALSE)

cat("\n\n--- Full model output (verbatim) ---\n\n")
cat("\n[Pre-DAA adjusted model]\n")
print(summary(cox_pre))
cat("\n[Post-DAA adjusted model]\n")
print(summary(cox_post))
cat("\n[All eras interaction + adjusted model]\n")
print(summary(cox_int_adj))

sink()

message("Saved: ", txt_file)

# =========================
# 4) (Optional) Also export as CSV for quick viewing
# =========================
csv_file <- file.path(out_dir, "SLK_Cox_PrePost_Interaction_Table.csv")
write.csv(all_tabs, csv_file, row.names = FALSE)
message("Saved: ", csv_file)

```

