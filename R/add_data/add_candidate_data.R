# Let's merge SLK with CAND_KIPA and CAND_LIIN


# ----Join SLK data set with CAND_KIPA----
cand_kipa <- cand_kipa %>% rename(PX_ID_KI = PX_ID)
tx_slk_final <- tx_slk_final %>% left_join(cand_kipa, by = "PX_ID_KI")%>%
  select(-ends_with(".y")) %>% 
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x"))%>%
  
  # ----Recode variables----
#recode diabetes variable  
  mutate(
    CAN_DIAB = case_when(
      CAN_DIAB == "998: UNKNOWN" ~ "Unknown",
      is.na(CAN_DIAB) ~ "Unknown",
      TRUE ~ CAN_DIAB
    )
  )%>%
#recode dialysis variable so that unknowns/NA combined 
  mutate(
    CAN_DIAL = case_when(
      CAN_DIAL %in% c(
        "998: Dialysis Status Unknown"
      ) ~ "Unknown",
      is.na(CAN_DIAL) ~ "Unknown",
      TRUE ~ CAN_DIAL
    )
  )%>%
#Calc candidate BMI, remove outliers 
  mutate(
    CAN_BMI_calc = CAN_WGT_KG / ( (CAN_HGT_CM/100)^2 ),
    CAN_BMI_clean = ifelse(CAN_BMI_calc < 10 | CAN_BMI_calc > 60, NA, CAN_BMI_calc)
  )%>% 
#create dialysis time in days variable, and create separate variable in years
    mutate(
    CAN_DIAL_DT = as.Date(CAN_DIAL_DT),
    REC_TX_DT_KI = as.Date(REC_TX_DT_KI),
    
    TIME_ON_DIALYSIS_DAYS = as.numeric(REC_TX_DT_KI - CAN_DIAL_DT),
    
    TIME_ON_DIALYSIS_DAYS = ifelse(
      TIME_ON_DIALYSIS_DAYS < 0,
      NA,
      TIME_ON_DIALYSIS_DAYS
    )
  ) %>% 
  mutate(
    TIME_ON_DIALYSIS_YEARS = TIME_ON_DIALYSIS_DAYS / 365.25
  ) %>% 
#clean candidate functional status variable
  mutate(
    # pull the leading numeric code before the colon
    CAN_FUNCTN_STAT_code = as.integer(str_extract(as.character(CAN_FUNCTN_STAT), "^\\d+")),
    
    # set pediatrics (4000s) + 996/998 to NA; otherwise keep original text
    CAN_FUNCTN_STAT_clean = case_when(
      CAN_FUNCTN_STAT_code %in% c(996L, 998L) ~ NA_character_,
      CAN_FUNCTN_STAT_code >= 4000L & CAN_FUNCTN_STAT_code < 5000L ~ NA_character_,
      TRUE ~ as.character(CAN_FUNCTN_STAT)
    )
  )

# ----keep original labels----
var_label(tx_slk_final$CAN_DIAL)<-var_label(cand_kipa$CAN_DIAL)
var_label(tx_slk_final$CAN_DIAB)<-var_label(cand_kipa$CAN_DIAB)
var_label(tx_slk_final$CAN_FUNCTN_STAT_clean)<-var_label(cand_kipa$CAN_FUNCTN_STAT)


table(tx_slk_final$CAN_FUNCTN_STAT_clean)


# ----merge CAND_LIIN and tx_slk_final, process variables ----
cand_liin <- cand_liin %>% 
  rename(PX_ID_LI = PX_ID) 

# list of variables to recode to 3 levels (Y,N,U)
vars_to_recode <- c(
  "CAN_PORTAL_VEIN",
  "CAN_TIPSS",
  "CAN_BACTERIA_PERIT",
  "CAN_ASCITES",
  "CAN_ENCEPH"
)

# ----Add liver candidate variables----
tx_slk_final <- tx_slk_final %>% 
  left_join(cand_liin, by = "PX_ID_LI") %>%
  select(-ends_with(".y")) %>% 
  rename_with(~ sub("\\.x$", "", .x), ends_with(".x")) %>%
  
  # ----Recode variables----
  # Recode Y/N/U vars and create *_recode
  mutate(across(
    all_of(vars_to_recode),
    ~ case_when(
        . == "Y" ~ "Y",
        . == "N" ~ "N",
        . %in% c("U", "", NA_character_) ~ "U",
        TRUE ~ "U"  # any unexpected value treated as unknown
    ),
    .names = "{.col}_recode"
  )) %>%
  
  # Convert all new recoded variables to factor with levels Y, N, U
  mutate(across(
    ends_with("_recode"),
    ~ factor(., levels = c("Y", "N", "U"))
  )) %>%
  # Carry over labels from the original variable to the *_recode versions
  mutate(across(
    ends_with("_recode"),
    ~ labelled::set_variable_labels(
        .,
        attr(
          pick(sub("_recode$", "", cur_column()))[[1]],
          "label"
        )
      )
  ))
