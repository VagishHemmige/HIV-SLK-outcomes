
# Let's merge SLK file with TXF_KI and TXF_KIs file

txf_li <- txf_li %>% rename(TRR_ID_LI = TRR_ID)
tx_slk_final_txf_li <- left_join(tx_slk_final, txf_li, by = "TRR_ID_LI")%>%
  select(-ends_with(".y")) %>% 
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>%
#convert "" to NAs
  mutate(TFL_FAIL_BILIARY = na_if(TFL_FAIL_BILIARY, "")) %>% 
  mutate(TFL_FAIL_INFECT = na_if(TFL_FAIL_INFECT, "")) %>% 
  mutate(TFL_FAIL_PRIME_GRAFT_FAIL = na_if(TFL_FAIL_PRIME_GRAFT_FAIL, "")) %>% 
  mutate(TFL_FAIL_PX_NONCOMP = na_if(TFL_FAIL_PX_NONCOMP, ""))

nrow(tx_slk_final_txf_li) # there are so many f/u records bc multiple records based on time since transplant
n_distinct(tx_slk_final_txf_li$PERS_ID)

txf_ki <- txf_ki %>% rename(TRR_ID_KI = TRR_ID)
tx_slk_final_txf_ki <- left_join(tx_slk_final, txf_ki, by = "TRR_ID_KI")%>%
  select(-ends_with(".y")) %>% 
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) 

table(tx_slk_final_txf_ki$TFL_GRAFT_STAT)

table(tx_slk_final_txf_li$TFL_FAIL_BILIARY)
table(tx_slk_final_txf_li$TFL_FAIL_INFECT)
sum(is.na(tx_slk_final_txf_li$TFL_FAIL_BILIARY))


