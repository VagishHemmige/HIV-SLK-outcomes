

# ---- Import raw kidney and liver files using the sRtr ----
tx_li <- load_srtr_file("tx_li", factor_labels = TRUE, var_labels = TRUE)
tx_ki <- load_srtr_file("tx_ki", factor_labels = TRUE, var_labels = TRUE)

# ----Import other files needed for the analysis----
donor_deceased <- load_srtr_file("donor_deceased", factor_labels = TRUE, var_labels = TRUE)
txf_li <- load_srtr_file("txf_li", factor_labels = TRUE, var_labels = TRUE)
txf_ki <- load_srtr_file("txf_ki", factor_labels = TRUE, var_labels = TRUE)
malig <- load_srtr_file("malig", factor_labels = TRUE, var_labels = TRUE)
immuno <- load_srtr_file("immuno", factor_labels = TRUE, var_labels = TRUE)
cand_kipa <- load_srtr_file("cand_kipa", factor_labels = TRUE, var_labels = TRUE)
cand_liin <- load_srtr_file("cand_liin", factor_labels = TRUE, var_labels = TRUE)
pra <- load_srtr_file("pra_hist", factor_labels = TRUE, var_labels = TRUE)



# ----Confirm that REC_TX_DT is in fact a date variable and throw an error otherwise----
if (!inherits(tx_li$REC_TX_DT, "Date")) {
  stop("`REC_TX_DT` must be a Date.", call. = FALSE)
}
if (!inherits(tx_ki$REC_TX_DT, "Date")) {
  stop("`REC_TX_DT` must be a Date.", call. = FALSE)
}

# ----Confirm that DONOR_ID is in the kidney and liver files----
if(!(c("DONOR_ID") %in% names(tx_li))){
  stop("`DONOR_ID` must be present in `tx_li", call. = FALSE)
}
if(!(c("DONOR_ID") %in% names(tx_ki))){
  stop("`DONOR_ID` must be present in `tx_ki", call. = FALSE)
}


# ----Check how many patients had multiple transplants----
tx_li %>% count(PERS_ID, REC_TX_ORG_TY) %>% filter(n > 1)%>%nrow()
tx_ki %>% count(PERS_ID, REC_TX_ORG_TY) %>% filter(n > 1)%>%nrow()

# ----complete FULL join by PERS_ID----
tx_merged <- full_join(tx_li, tx_ki, by = "PERS_ID", suffix = c("_LI", "_KI"))

# ----diagnostic joins ----
# Liver patients not in kidney
only_in_li <- anti_join(tx_li, tx_ki, by = "PERS_ID")

# Kidney patients not in liver
only_in_ki <- anti_join(tx_ki, tx_li, by = "PERS_ID")

#matched patients
matched <- inner_join(tx_li, tx_ki, by = "PERS_ID")

# ----checks for date----
n_distinct(matched$PERS_ID)
summary(abs(as.numeric(as.Date(matched$REC_TX_DT.x) - as.Date(matched$REC_TX_DT.y))))

# ----calculate counts----
message("Only in liver:", nrow(only_in_li), "\n")
message("Only in kidney:", nrow(only_in_ki), "\n")
message("In both (SLK candidates):", nrow(matched), "\n")

# ----filter SLK patients, who had transplants within 1 day of each other to find SLKs----
tx_slkRaw <- tx_merged %>%
  filter(!is.na(REC_TX_DT_LI) & !is.na(REC_TX_DT_KI)) %>%
  filter(abs(difftime(REC_TX_DT_LI, REC_TX_DT_KI, units = "days")) <= 1)

# ----Exact duplicates after filtering----
exact_duplicates<-tx_slkRaw %>%
  add_count(PERS_ID, REC_TX_DT_LI, REC_TX_DT_KI, name = "N") %>%
  filter(N > 1) %>%
  arrange(PERS_ID, REC_TX_DT_LI, REC_TX_DT_KI)

# ----Investigating the nature of the duplicates----
message("Patient #1: # 2 liver donors, Rec neg")
message("Patient #2: #this looks like a duplicate entry but need to confirm")
message("Patient #3: # 2 liver donors, rec neg")

