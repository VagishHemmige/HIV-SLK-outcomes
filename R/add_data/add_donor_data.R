# Now I want to add in the donor_deceased file, combine with the slk dataset, and process those variables

# merge in donor file 
tx_slk_final <- tx_slk_final %>%
  left_join(donor_deceased, by = "DONOR_ID") %>%
  select(-ends_with(".y")) %>% 
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>% 
  
  # recode donor HIV NAT variable
  mutate(DON_HIV_NAT_RECODED = case_when(
    DON_HIV_NAT == "P" ~ "Positive",
    DON_HIV_NAT == "N" ~ "Negative",
    TRUE ~ NA_character_
  )) %>% 
  
  # recode donor HCV status
  mutate(DON_HCV_STAT_RECODED = case_when(
    DON_HCV_STAT == "1: Positive" ~ "Positive",
    DON_HCV_STAT == "2: Negative" ~ "Negative",
    TRUE ~ NA_character_
  )) %>% 
  
  # recode donor anti-HCV Ab
  mutate(DON_ANTI_HCV_RECODED = case_when(
    DON_ANTI_HCV == "P: Positive" ~ "Positive",
    DON_ANTI_HCV == "N: Negative" ~ "Negative",
    TRUE ~ NA_character_
  )) %>% 
  
  # recode donor Hep B core Ab
  mutate(DON_HBC_STAT_RECODED = case_when(
    DON_HBC_STAT == "1: Positive" ~ "Positive",
    DON_HBC_STAT == "2: Negative" ~ "Negative",
    TRUE ~ NA_character_
  )) %>% 
  
  # recode donor HIV Ab
  mutate(
    DON_ANTI_HIV_recode = case_when(
      DON_ANTI_HIV == "P: Positive" ~ "P",
      DON_ANTI_HIV == "N: Negative" ~ "N",
      TRUE ~ NA_character_
    ),
    DON_ANTI_HIV_recode = factor(DON_ANTI_HIV_recode, levels = c("P", "N"))
  )



# Now I want to add in donor_deceased file, combine with liver-only dataset, and process these variables
#merge in donor file. have to make sure it merged correctly because there is missing data 
tx_li_only_final<-left_join(, donor_deceased, by = "DONOR_ID") %>% 
  select(-ends_with(".y")) %>% 
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>% 
  #make donor HIV NAT variable binary
  mutate(DON_HIV_NAT_POS=ifelse(DON_HIV_NAT=="P", 1, 0)) %>% 
  #make donor HCV stat binary
  mutate(DON_HCV_POS=ifelse(DON_HCV_STAT=="1: Positive", 1, 0)) %>% 
  #make donor HCV Ab stat binary (note to self, used column from donor deceased but equivalent to SLK donor variables)
  mutate(DON_HCV_AB_POS=ifelse(DON_ANTI_HCV=="P: Positive", 1, 0)) %>% 
  #make donor Hep B core Ab positive binary variab;e
  mutate(DON_HBC_POS=ifelse(DON_HBC_STAT=="1: Positive", 1, 0))

#come back to this because i am confused. below is seeing how many of missing donor info comes from rec HIV + 
missing_donors <- tx_li_only_final_don %>%
  filter(is.na(DON_HIV_NAT)) %>%
  select(DONOR_ID) %>%
  distinct()
table(tx_li_only_final_don$DON_HCV_POS)



