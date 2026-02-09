# Let's combine liver and kidney variables




# ----using combine_li_ki_consistent() function to combine variables----
tx_slk_clean <- tx_slk_clean %>%
  mutate(
    # HTN
    CAN_DRUG_TREAT_HYPERTEN_COMB = combine_li_ki_consistent(
      CAN_DRUG_TREAT_HYPERTEN_LI,
      CAN_DRUG_TREAT_HYPERTEN_KI,
      pos_codes = "Y",
      neg_codes = "N",
      out_pos = "Y",
      out_neg = "N"
    ),

    # CMV
    REC_CMV_COMBINED = combine_li_ki_consistent(
      REC_CMV_STAT_LI,
      REC_CMV_STAT_KI,
      pos_codes = "P",
      neg_codes = "N",
      out_pos = "P",
      out_neg = "N"
    ),

    # HCV
    REC_HCV_STAT_COMB = combine_li_ki_consistent(
      REC_HCV_STAT_LI,
      REC_HCV_STAT_KI,
      pos_codes = "P",
      neg_codes = "N",
      out_pos = "P",
      out_neg = "N"
    ),

    # HBV Ab
    REC_HBV_ANTIBODY_COMB = combine_li_ki_consistent(
      REC_HBV_ANTIBODY_LI,
      REC_HBV_ANTIBODY_KI,
      pos_codes = "P",
      neg_codes = "N",
      unknown_codes = c("U", "ND", "Unknown", "", NA),
      out_pos = "P",
      out_neg = "N"
    ),

    # HBV surface antigen
    REC_HBV_SURF_ANTIGEN_COMB = combine_li_ki_consistent(
      REC_HBV_SURF_ANTIGEN_LI,
      REC_HBV_SURF_ANTIGEN_KI,
      pos_codes = "P",
      neg_codes = "N",
      unknown_codes = c("U", "ND", "Unknown", "", NA),
      out_pos = "P",
      out_neg = "N"
    ),

    # hepatocellular carcinoma
    REC_MALIG_TY_HEPCARCINOMA_COMB = combine_li_ki_consistent(
      REC_MALIG_TY_HEPCARCINOMA_LI,
      REC_MALIG_TY_HEPCARCINOMA_KI,
      pos_codes = "1",
      neg_codes = "0",
      unknown_codes = c("", NA),
      out_pos = "Yes",
      out_neg = "No"
    ),

    # candidate malignancy
    CAN_MALIG_COMB = combine_li_ki_consistent(
      CAN_MALIG_LI,
      CAN_MALIG_KI,
      pos_codes = "Y",
      neg_codes = "N",
      unknown_codes = c("U", "", NA),
      out_pos = "Yes",
      out_neg = "No"
    ),

    # liver malignancy
    REC_MALIG_TY_LIVER_COMB = combine_li_ki_consistent(
      REC_MALIG_TY_LIVER_LI,
      REC_MALIG_TY_LIVER_KI,
      pos_codes = "1",
      neg_codes = "0",
      unknown_codes = c("U", "", NA),  # or c("", NA) if no "U"
      out_pos = "Yes",
      out_neg = "No"
    )
  )


# ----count discordant cases----

# Discordant HCV status between liver and kidney data sets
sum(
  (tx_slk_clean$REC_HCV_STAT_LI == "P" & tx_slk_clean$REC_HCV_STAT_KI == "N") |
  (tx_slk_clean$REC_HCV_STAT_LI == "N" & tx_slk_clean$REC_HCV_STAT_KI == "P"),
  na.rm = TRUE
)

# Discordant HBV surface antigen status between liver and kidney data sets
sum(
  (tx_slk_clean$REC_HBV_SURF_ANTIGEN_LI == "P" & tx_slk_clean$REC_HBV_SURF_ANTIGEN_KI == "N") |
  (tx_slk_clean$REC_HBV_SURF_ANTIGEN_LI == "N" & tx_slk_clean$REC_HBV_SURF_ANTIGEN_KI == "P"),
  na.rm = TRUE
)

# ----attach labels ----
var_label(tx_slk_clean$REC_CMV_COMBINED)<- var_label(tx_slk_clean$REC_CMV_STAT_LI)
var_label(tx_slk_clean$CAN_DRUG_TREAT_HYPERTEN_COMB)<-var_label(tx_slk_clean$CAN_DRUG_TREAT_HYPERTEN_LI)
var_label(tx_slk_clean$REC_HBV_ANTIBODY_COMB)<- var_label(tx_slk_clean$REC_HBV_ANTIBODY_LI)
var_label(tx_slk_clean$REC_HBV_SURF_ANTIGEN_COMB)<-var_label(tx_slk_clean$REC_HBV_SURF_ANTIGEN_LI)
var_label(tx_slk_clean$REC_HCV_STAT_COMB)<- var_label(tx_slk_clean$REC_HCV_STAT_LI)
var_label(tx_slk_clean$REC_MALIG_TY_HEPCARCINOMA_COMB)<-var_label(tx_slk_clean$REC_MALIG_TY_HEPCARCINOMA_LI)
var_label(tx_slk_clean$REC_MALIG_TY_LIVER_COMB)<-var_label(tx_slk_clean$REC_MALIG_TY_LIVER_LI)
var_label(tx_slk_clean$CAN_MALIG_COMB)<-var_label(tx_slk_clean$CAN_MALIG_LI)
