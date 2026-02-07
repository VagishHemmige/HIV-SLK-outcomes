#Let's merge SLK file with immuno file 
#don't need immuno for liver unclear if need for kideny, leave for now 
#combine TRR_ID between liver and kidney
tx_slk_final_im<-tx_slk_final %>% 
  mutate(TRR_ID = coalesce(TRR_ID_LI,TRR_ID_KI)) %>% 
  #merge immuno dataset with tx_slk_final
  left_join(immuno, by = "TRR_ID")%>%
  select(-ends_with(".y")) %>% 
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) 
