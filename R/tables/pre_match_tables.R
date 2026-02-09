#Tables for pre-matching


# ----Let's perform univariate analysis of final SLK data set, HIV slk vs not-HIV SLK, SLK HIV vs Liver only HIV----

# ----make lists of variables of interest----
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


# ----Create donor characteristics table ----
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
  gt::gtsave("tables/don_table.png")


# ----create recipient table for liver variables ----
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
  gt::gtsave("tables/rec_li_table.png")

# ----Create recipient table for kidney variables ----
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
  gt::gtsave("tables/rec_ki_table.png")

# ----create table for candidate liver variables ----
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
  gt::gtsave("tables/cand_li_table.png")


# ----Create table for candidate kidney variables ----
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
  gt::gtsave("tables/cand_ki_table.png")
