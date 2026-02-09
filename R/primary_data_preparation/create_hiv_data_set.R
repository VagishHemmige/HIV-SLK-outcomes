#Creating SLK HIV data set 


# ----Discrepancies between HIV status in kidney and liver data set----
table(
  tx_slk_clean$REC_HIV_STAT_LI,
  tx_slk_clean$REC_HIV_STAT_KI,
  useNA = "ifany"
)

discordant_HIV_raw <- tx_slk_clean %>%
  filter(
    (REC_HIV_STAT_LI == "P" & REC_HIV_STAT_KI == "N") |
      (REC_HIV_STAT_LI == "N" & REC_HIV_STAT_KI == "P")
  )

nrow(discordant_HIV_raw)


# ----turn unknowns and ND into NAs----
tx_slk_step1 <- tx_slk_clean %>%
  mutate(
    REC_HIV_STAT_LI_clean = na_if(str_trim(REC_HIV_STAT_LI), ""),
    REC_HIV_STAT_KI_clean = na_if(str_trim(REC_HIV_STAT_KI), ""),
    
    REC_HIV_STAT_LI_clean = ifelse(
      REC_HIV_STAT_LI_clean %in% c("ND", "Unknown", "U"), NA, REC_HIV_STAT_LI_clean
    ),
    REC_HIV_STAT_KI_clean = ifelse(
      REC_HIV_STAT_KI_clean %in% c("ND", "Unknown", "U"), NA, REC_HIV_STAT_KI_clean
    )
  )

# ----flag discordant cases----
tx_slk_step2 <- tx_slk_step1 %>%
  mutate(
    HIV_DISCORDANT = case_when(
      (REC_HIV_STAT_LI_clean == "P" & REC_HIV_STAT_KI_clean == "N") |
        (REC_HIV_STAT_LI_clean == "N" & REC_HIV_STAT_KI_clean == "P") ~ 1,
      TRUE ~ 0
    )
  )

excluded_count <- tx_slk_step2 %>%
  filter(HIV_DISCORDANT == 1) %>%
  nrow()

excluded_count #16 discordant cases 


# ----exclude discordant P and N cases----
tx_slk_final <- tx_slk_step2 %>%
  filter(HIV_DISCORDANT == 0) %>%
  mutate(
    HIV_POSITIVE = case_when(
      # BOTH positive
      REC_HIV_STAT_LI_clean == "P" & REC_HIV_STAT_KI_clean == "P" ~ 1,
      
      # One positive, other missing/unknown
      REC_HIV_STAT_LI_clean == "P" & is.na(REC_HIV_STAT_KI_clean) ~ 1,
      REC_HIV_STAT_KI_clean == "P" & is.na(REC_HIV_STAT_LI_clean) ~ 1,
      
      # Everything else
      TRUE ~ 0
    )
  )

table(tx_slk_final$HIV_POSITIVE)
table(
  tx_slk_final$REC_HIV_STAT_LI_clean,
  tx_slk_final$REC_HIV_STAT_KI_clean,
  tx_slk_final$HIV_POSITIVE,
  useNA = "ifany"
)




