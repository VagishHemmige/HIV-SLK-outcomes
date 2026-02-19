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

#----save dataset for matching, export to baseR ----
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


if (FALSE){
# ----export match back into Rstudio----
matched_obj <- readRDS("~/Desktop/slk_match/matched_cmv_hbv_era_exact_txcal365.rds")

dim(matched_obj)
table(matched_obj$HIV_POSITIVE)
summary(matched_obj$weights)
"subclass" %in% names(matched_obj)   # should be TRUE

#----build matched dataset----

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


#Labeling
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

}