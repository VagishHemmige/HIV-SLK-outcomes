

#Import raw kidney and liver files using the sRtr
tx_li <- load_srtr_file("tx_li", factor_labels = TRUE, var_labels = TRUE)
tx_ki <- load_srtr_file("tx_ki", factor_labels = TRUE, var_labels = TRUE)

#Confirm that REC_TX_DT is in fact a date variable and throw an error otherwise
if (!inherits(tx_li$REC_TX_DT, "Date")) {
  stop("`REC_TX_DT` must be a Date.", call. = FALSE)
}
if (!inherits(tx_ki$REC_TX_DT, "Date")) {
  stop("`REC_TX_DT` must be a Date.", call. = FALSE)
}

#Confirm that DONOR_ID is in the kidney and liver files
if(!(c("DONOR_ID") %in% names(tx_li))){
  stop("`DONOR_ID` must be present in `tx_li", call. = FALSE)
}
if(!(c("DONOR_ID") %in% names(tx_ki))){
  stop("`DONOR_ID` must be present in `tx_ki", call. = FALSE)
}


#Check how many patients had multiple transplants
tx_li %>% count(PERS_ID, REC_TX_ORG_TY) %>% filter(n > 1)%>%nrow()
tx_ki %>% count(PERS_ID, REC_TX_ORG_TY) %>% filter(n > 1)%>%nrow()

#complete FULL join by PERS_ID
tx_merged <- full_join(tx_li, tx_ki, by = "PERS_ID", suffix = c("_LI", "_KI"))

#diagnostic joins 
# Liver patients not in kidney
only_in_li <- anti_join(tx_li, tx_ki, by = "PERS_ID")

# Kidney patients not in liver
only_in_ki <- anti_join(tx_ki, tx_li, by = "PERS_ID")

#matched patients
matched <- inner_join(tx_li, tx_ki, by = "PERS_ID")

#checks for date
n_distinct(matched$PERS_ID)
summary(abs(as.Date(matched$REC_TX_DT.x) - as.Date(matched$REC_TX_DT.y)))

#calculate counts
cat("Only in liver:", nrow(only_in_li), "\n")
cat("Only in kidney:", nrow(only_in_ki), "\n")
cat("In both (SLK candidates):", nrow(matched), "\n")

#filter out SLK patients, who had transplants within 1 day of each other to find SLKs
tx_slkRaw <- tx_merged %>%
  filter(!is.na(REC_TX_DT_LI) & !is.na(REC_TX_DT_KI)) %>%
  filter(abs(difftime(REC_TX_DT_LI, REC_TX_DT_KI, units = "days")) <= 1)

# Exact duplicates after filtering
tx_slkRaw %>%
  count(PERS_ID, REC_TX_DT_LI, REC_TX_DT_KI) %>%
  filter(n > 1)
#checking what these duplicates are all about
tx_slkRaw %>% 
  filter(PERS_ID==2117076	) # 2 liver donors, Rec neg
tx_slkRaw %>% 
  filter(PERS_ID==3905643	) #this looks like a duplicate entry but need to confirm 
tx_slkRaw %>% 
  filter(PERS_ID==4505523		) %>% 
  select(REC_HIV_STAT_LI)# 2 liver donors, rec neg

