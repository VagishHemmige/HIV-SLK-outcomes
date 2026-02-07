
#Let's merge SLK with PRA data set

pra <- pra %>% rename(PX_ID_KI = PX_ID)
tx_slk_final_pra <- left_join(tx_slk_final, pra, by = "PX_ID_KI")%>%
  select(-ends_with(".y")) %>% 
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x"))

pra %>% count(PX_ID_KI) %>% filter(n > 1)

sum(duplicated(pra$PX_ID_KI))

pra %>%
  semi_join(tx_slk_final, by = "PX_ID_KI") %>%
  count(PX_ID_KI) %>%
  filter(n > 1)

