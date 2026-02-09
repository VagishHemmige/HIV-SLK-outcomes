#Manuscript Matching




# ----prepare matching data set----
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



# ----convert transplant date to numeric and check----
match_df$REC_TX_DT <- as.Date(match_df$REC_TX_DT)
match_df$REC_TX_DT_num <- as.numeric(match_df$REC_TX_DT)
summary(match_df$REC_TX_DT)
summary(match_df$REC_TX_DT_num)


#turn NAs into "Missing"
# After: match_df <- readRDS("...")


# ----Apply make_missing_level() to the covariates MatchIt complained about----
match_df$REC_HCV_STAT_COMB <- make_missing_level(match_df$REC_HCV_STAT_COMB, "Missing")
match_df$REC_CMV_COMBINED <- make_missing_level(match_df$REC_CMV_COMBINED, "Missing")
match_df$REC_HBV_SURF_ANTIGEN_COMB <- make_missing_level(match_df$REC_HBV_SURF_ANTIGEN_COMB, "Missing")
match_df$REC_HBV_ANTIBODY_COMB <- make_missing_level(match_df$REC_HBV_ANTIBODY_COMB, "Missing")

#save dataset for matching, export to baseR 
# Save updated dataset for base R matching
saveRDS(match_df, "data-private/match_df_for_baseR_cmv_hbv.rds")

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



