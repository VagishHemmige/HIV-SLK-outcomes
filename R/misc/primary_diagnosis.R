# Now need to label categories for candidate primary diagnosis 

#---------------------------------------------
# 1. Extract unique values for kidney diagnoses
#---------------------------------------------
kidney_codes <- tx_slk_final %>%
  distinct(REC_DGN_KI) %>%
  filter(!is.na(REC_DGN_KI)) %>%
  mutate(
    Code = as.numeric(str_extract(REC_DGN_KI, "^[0-9]+")),
    Diagnosis = str_trim(str_remove(REC_DGN_KI, "^[0-9]+:\\s*"))
  ) %>%
  arrange(Code) %>%
  select(Code, Diagnosis)

#---------------------------------------------
# 2. Extract unique values for liver diagnoses
#---------------------------------------------
liver_codes <- tx_slk_final %>%
  distinct(REC_DGN_LI) %>%
  filter(!is.na(REC_DGN_LI)) %>%
  mutate(
    Code = as.numeric(str_extract(REC_DGN_LI, "^[0-9]+")),
    Diagnosis = str_trim(str_remove(REC_DGN_LI, "^[0-9]+:\\s*"))
  ) %>%
  arrange(Code) %>%
  select(Code, Diagnosis)

#---------------------------------------------
# 3. Export both lists to Excel (two sheets)
#---------------------------------------------
write_xlsx(
  list(
    "Kidney" = kidney_codes,
    "Liver" = liver_codes
  ),
  "Diagnosis_Code_Lists_2.xlsx"
)


