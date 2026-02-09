#Let's process the SLK data


# ----Create a number of new variables----
tx_slk_final <- tx_slk_final %>%
  #exclude patients with mismatched donors
  filter(DONOR_ID_LI == DONOR_ID_KI) %>% 
  #Calculate Liver and kidney donor BMI
  mutate(DON_BMI_LI=DON_WGT_KG_LI/(DON_HGT_CM_LI/100)^2) %>% 
  mutate(DON_BMI_KI=DON_WGT_KG_KI/(DON_HGT_CM_KI/100)^2) %>% 
  #Turn liver failure date into a failure/censor date, with a separate variable to 
  #determine which of the two applies
  mutate(TFL_GRAFT_DT_LI_censor=if_else(is.na(TFL_GRAFT_DT_LI), 
                                        TFL_LAFUDATE_LI,
                                        TFL_GRAFT_DT_LI))%>%
  mutate(TFL_GRAFT_DT_LI_binary=ifelse(is.na(TFL_GRAFT_DT_LI),0,1))%>%
  mutate(TFL_GRAFT_DT_LI_yrs = (1/365)+
           as.numeric(
             difftime(TFL_GRAFT_DT_LI_censor, 
                      REC_TX_DT_LI, 
                      units = "days")) / 365.25) %>% 
  #Create median time on waitlist until transplant variable
  mutate(median_waitlist_time_years_li=as.numeric(difftime(REC_TX_DT_LI,
                                                           CAN_LISTING_DT_LI,
                                                           units="days"))/365.25) %>% 
  #Turn kidney failure date into a failure/censor date, with a separate variable to 
  #determine which of the two applies
  mutate(TFL_GRAFT_DT_KI_censor=if_else(is.na(TFL_GRAFT_DT_KI), 
                                        TFL_LAFUDATE_KI,
                                        TFL_GRAFT_DT_KI))%>%
  mutate(TFL_GRAFT_DT_KI_binary=ifelse(is.na(TFL_GRAFT_DT_KI),0,1))%>%
  mutate(TFL_GRAFT_DT_KI_yrs = (1/365)+
           as.numeric(
             difftime(TFL_GRAFT_DT_KI_censor, 
                      REC_TX_DT_KI, 
                      units = "days")) / 365.25) %>% 
  
  #create combined kidney and liver time to death variable 
  mutate(
    # 1) One transplant date (earliest of the two, just in case)
    REC_TX_DT_COMPOSITE = pmin(REC_TX_DT_LI, REC_TX_DT_KI, na.rm = TRUE),
    
    # 2) One last-follow-up date (latest follow-up observed)
    TFL_LAFUDATE_COMPOSITE = pmax(TFL_LAFUDATE_LI, TFL_LAFUDATE_KI, na.rm = TRUE),
    
    # 3) One composite death date: OPTN > SSA > TFL, using whichever organ has it
    PERS_OPTN_DEATH_DT_COMPOSITE = coalesce(PERS_OPTN_DEATH_DT_LI, PERS_OPTN_DEATH_DT_KI),
    PERS_SSA_DEATH_DT_COMPOSITE  = coalesce(PERS_SSA_DEATH_DT_LI,  PERS_SSA_DEATH_DT_KI),
    TFL_DEATH_DT_COMPOSITE       = coalesce(TFL_DEATH_DT_LI,       TFL_DEATH_DT_KI),
    
    REC_DEATH_DT_COMPOSITE = coalesce(
      PERS_OPTN_DEATH_DT_COMPOSITE,
      PERS_SSA_DEATH_DT_COMPOSITE,
      TFL_DEATH_DT_COMPOSITE
    ),
    
    # Optional: where did the death date come from?
    REC_DEATH_SOURCE = case_when(
      !is.na(PERS_OPTN_DEATH_DT_COMPOSITE) ~ "OPTN",
      is.na(PERS_OPTN_DEATH_DT_COMPOSITE) & !is.na(PERS_SSA_DEATH_DT_COMPOSITE) ~ "SSA",
      is.na(PERS_OPTN_DEATH_DT_COMPOSITE) & is.na(PERS_SSA_DEATH_DT_COMPOSITE) & !is.na(TFL_DEATH_DT_COMPOSITE) ~ "TFL",
      TRUE ~ NA_character_
    ),
    
    # 4) Censoring + event indicator
    REC_DEATH_DT_CENSOR = if_else(
      is.na(REC_DEATH_DT_COMPOSITE),
      TFL_LAFUDATE_COMPOSITE,
      REC_DEATH_DT_COMPOSITE
    ),
    REC_DEATH_BINARY = ifelse(is.na(REC_DEATH_DT_COMPOSITE), 0, 1),
    
    # 5) Time-to-death (years). Add (1/365) if you want to avoid 0 time.
    REC_DEATH_yrs = as.numeric(difftime(REC_DEATH_DT_CENSOR, REC_TX_DT_COMPOSITE, units = "days")) / 365.25
  ) %>% 
  #Create median time on waitlist until transplant variable
  mutate(median_waitlist_time_years_ki=as.numeric(difftime(REC_TX_DT_KI,
                                                           CAN_LISTING_DT_KI,
                                                           units="days"))/365.25) %>% 
  #combine categorical recipient age into one variable, preferring liver data first, then kidney, then NA if neither have data
  mutate(
    # Unified recipient age category 
    REC_AGE_AT_TX = case_when(
      !is.na(REC_AGE_AT_TX_LI) ~ REC_AGE_AT_TX_LI,
      is.na(REC_AGE_AT_TX_LI) & !is.na(REC_AGE_AT_TX_KI) ~ REC_AGE_AT_TX_KI,  
      TRUE ~ NA_character_
    )
  ) %>%  
  #create numeric recipient age, combine into one variable and convert from months to years
  mutate(
    REC_AGE_YEARS = coalesce(REC_AGE_IN_MONTHS_AT_TX_LI, REC_AGE_IN_MONTHS_AT_TX_KI) / 12
  ) %>% 
  #combine recipient gender into one variable, preferring liver data first 
  mutate(
    CAN_GENDER = case_when(
      !is.na(CAN_GENDER_LI) ~ CAN_GENDER_LI,      
      is.na(CAN_GENDER_LI) & !is.na(CAN_GENDER_KI) ~ CAN_GENDER_KI,  
      TRUE ~ NA_character_                          
    )
  ) %>% 
  #Create numeric candidate age at listing variable
  mutate(
    CAN_AGE_AT_LISTING_YR_LI = CAN_AGE_IN_MONTHS_AT_LISTING_LI/12
  )%>% 
  mutate(
    CAN_AGE_AT_LISTING_YR_KI = CAN_AGE_IN_MONTHS_AT_LISTING_KI/12
  ) %>% 
  #combine candidate race at listing into one variable, preferring kidney data first
  mutate(
    CAN_RACE = case_when(
      !is.na(CAN_RACE_LI) ~ CAN_RACE_LI,                             
      is.na(CAN_RACE_LI) & !is.na(CAN_RACE_KI) ~ CAN_RACE_KI,         
      TRUE ~ NA_character_                                          
    )
  ) %>% 
  #combine donor race for kidney/liver
  mutate(
    DON_RACE = case_when(
      !is.na(DON_RACE_LI) ~ DON_RACE_LI,                             
      is.na(DON_RACE_LI) & !is.na(DON_RACE_KI) ~ DON_RACE_KI,         
      TRUE ~ NA_character_                                          
    ) 
  )%>% 
  #combine donor ethnicity for kidney/liver
  mutate(
    DON_ETHNICITY_SRTR = case_when(
      !is.na(DON_ETHNICITY_SRTR_LI) ~ DON_ETHNICITY_SRTR_LI,                             
      is.na(DON_ETHNICITY_SRTR_LI) & !is.na(DON_ETHNICITY_SRTR_KI) ~ DON_ETHNICITY_SRTR_KI,       
      TRUE ~ NA_character_                                          
    ) 
  )%>% 
  #combine donor death mechanism for kidney/liver
  mutate(
    DON_DEATH_MECH = case_when(
      !is.na(DON_DEATH_MECH_LI) ~ DON_DEATH_MECH_LI,                             
      is.na(DON_DEATH_MECH_LI) & !is.na(DON_DEATH_MECH_KI) ~ DON_DEATH_MECH_KI,       
      TRUE ~ NA_character_                                          
    ) 
  )%>% 
  #combine death mech categories:
  mutate(
    DON_DEATH_MECH_CAT = case_when(
      
      # Trauma / external insult
      DON_DEATH_MECH %in% c(
        "7: GUNSHOT WOUND",
        "8: STAB",
        "9: BLUNT INJURY",
        "995: 995-Gunshot/stab wound (Pre-OTIS)"
      ) ~ "trauma/external insult",
      
      # Drug intoxication
      DON_DEATH_MECH == "3: DRUG INTOXICATION" ~ "drug intoxication",
      
      # Other (medical / non-external)
      DON_DEATH_MECH %in% c(
        "11: INTRACRANIAL HEMORRHAGE/STROKE",
        "12: DEATH FROM NATURAL CAUSES",
        "5: CARDIOVASCULAR",
        "1: DROWNING",
        "2: SEIZURE",
        "4: ASPHYXIATION",
        "6: ELECTRICAL",
        "997: NONE OF THE ABOVE"
      ) ~ "other",
      
      # Catch anything unexpected
      TRUE ~ NA_character_
    )
  ) %>% 
  #combine donor age for kidney/liver
  mutate(
    DON_AGE = coalesce(DON_AGE_LI, DON_AGE_KI)
  ) %>% 
  #combine donor gender for kidney/liver
  mutate(
    DON_GENDER = case_when(
      !is.na(DON_GENDER_LI) ~ DON_GENDER_LI,                             
      is.na(DON_GENDER_LI) & !is.na(DON_GENDER_KI) ~ DON_GENDER_KI,       
      TRUE ~ NA_character_                                          
    ) 
  )%>% 
  #combine expanded donor criteria for kidney/li
  mutate(
    DON_EXPAND_DON_KI = coalesce(DON_EXPAND_DON_KI_LI, DON_EXPAND_DON_KI_KI)
  ) %>% 
  #combine donor ID for kidney/liver
  mutate(
    DONOR_ID = coalesce(DONOR_ID_LI, DONOR_ID_KI)
  ) %>% 
  #transplant reason liver used OPTN diagnosis categories https://optn.transplant.hrsa.gov/patients/by-organ/liver/
  mutate(
    REC_DGN_LI_num = as.numeric(str_extract(as.character(REC_DGN_LI), "^[0-9]+")),
    REC_DGN_KI_num = as.numeric(str_extract(as.character(REC_DGN_KI), "^[0-9]+"))
  ) %>% 
  mutate(
    TRANSPLANT_REASON_LI = case_when(
      # Explicit missing category (instead of NA)
      is.na(REC_DGN_LI_num) ~ "Missing",
      
      # Acute liver failure / acute hepatitis
      REC_DGN_LI_num %in% c(
        4100, 4101, 4102, 4104, 4105, 4106, 4108, 4110, 4217
      ) ~ "Acute Liver Failure/Hepatitis",
      
      # Chronic liver disease
      REC_DGN_LI_num %in% c(
        4220, 4230, 4231, 4235, 4240, 4241, 4242, 4245,
        4200, 4201, 4202, 4204, 4205, 4206, 4207, 4208,
        4209, 4210, 4212, 4213, 4214, 4215, 4216
      ) ~ "Chronic Liver Disease",
      
      # Malignant neoplasms
      REC_DGN_LI_num %in% c(
        4400, 4401, 4403, 4404, 4410, 4430
      ) ~ "Malignant Neoplasms",
      
      # Other known liver diagnoses
      REC_DGN_LI_num %in% c(
        4270, 4271, 4272, 4275,
        4300, 4301, 4302, 4303, 4307, 4315,
        999, 4250, 4255, 4260, 4264, 4265,
        4280, 4285, 4290, 4451, 4455, 4500, 4520
      ) ~ "Other",
      
      # Any remaining known-but-unlisted codes
      TRUE ~ "Other"
    )
  ) %>% 
  #transplant reason kidney (used OPTN diagnosis categories https://optn.transplant.hrsa.gov/patients/by-organ/kidney/)
  mutate(
    TRANSPLANT_REASON_KI = case_when(
      is.na(REC_DGN_KI_num) ~ "Missing",
      
      REC_DGN_KI_num %in% c(3010, 3014, 3015, 3025, 3028, 3036, 3052) ~
        "Congenital, Rare Familial,and Metabolic Disorders",
      
      REC_DGN_KI_num %in% c(3011, 3012, 3038, 3039, 3069, 3070, 3071) ~
        "Diabetes",
      
      REC_DGN_KI_num %in% c(3000, 3001, 3002, 3003, 3004, 3005, 3006, 3016, 3018, 3019,
                            3024, 3029, 3031, 3033, 3035, 3041, 3042, 3043, 3054, 3064,
                            3068, 3074) ~
        "Glomerular Disease",
      
      REC_DGN_KI_num %in% c(3040) ~
        "Hypertensive Nephrosclerosis",
      
      REC_DGN_KI_num %in% c(3058, 3020, 3021, 3022) ~
        "Neoplasms",
      
      REC_DGN_KI_num %in% c(999, 3032, 3056) ~
        "Other Kidney",
      
      REC_DGN_KI_num %in% c(3008) ~
        "Polycystic Kidneys",
      
      REC_DGN_KI_num %in% c(3072) ~
        "Hepatorenal syndrome",
      
      REC_DGN_KI_num %in% c(3034, 3050, 3051, 3053, 3055) ~
        "Renovascular and Other Vascular Diseases",
      
      REC_DGN_KI_num %in% c(3007, 3009, 3013, 3026, 3027, 3030, 3044, 3045, 3046, 3047,
                            3048, 3049, 3057, 3059, 3060, 3063, 3073) ~
        "Tubular and Interstitial Diseases",
      
      TRUE ~ "Other"
    )
  ) %>%
  #collapse kidney variable into smaller categories 
  mutate(
    TRANSPLANT_REASON_KI_COLLAPSED = case_when(
      TRANSPLANT_REASON_KI == "Missing" ~ "Missing",
      
      TRANSPLANT_REASON_KI == "Diabetes" ~ "Diabetes",
      TRANSPLANT_REASON_KI == "Glomerular Disease" ~ "Glomerular disease",
      
      TRANSPLANT_REASON_KI %in% c("Hypertensive Nephrosclerosis",
                                  "Renovascular and Other Vascular Diseases") ~
        "Hypertensive / vascular",
      
      TRANSPLANT_REASON_KI %in% c("Polycystic Kidneys",
                                  "Congenital, Rare Familial,and Metabolic Disorders") ~
        "Polycystic / hereditary",
      
      TRANSPLANT_REASON_KI == "Hepatorenal syndrome" ~ "Hepatorenal syndrome",
      
      TRANSPLANT_REASON_KI %in% c("Tubular and Interstitial Diseases",
                                  "Other Kidney",
                                  "Other") ~
        "Tubulointerstitial / other CKD",
      
      TRANSPLANT_REASON_KI %in% c("Neoplasms") ~ "Rare / other",
      
      TRUE ~ "Rare / other"
    )
  ) %>% 
  mutate(
    TRANSPLANT_REASON_KI_COLLAPSED = factor(
      TRANSPLANT_REASON_KI_COLLAPSED,
      levels = c(
        "Diabetes",
        "Glomerular disease",
        "Hypertensive / vascular",
        "Polycystic / hereditary",
        "Hepatorenal syndrome",
        "Tubulointerstitial / other CKD",
        "Rare / other",
        "Missing"
      )
    )
  ) %>% 
  #create year of transplant variable 
  mutate(
    REC_TX_DT = coalesce(REC_TX_DT_LI, REC_TX_DT_KI),
    YEAR_TRANSPLANT = year(REC_TX_DT)
  ) %>% 
  #create DAA era varible
  mutate(
    ERA = ifelse(YEAR_TRANSPLANT< 2014, "Pre-DAA", "Post-DAA"),
    ERA = factor(ERA, levels = c("Pre-DAA", "Post-DAA"))
  ) %>% 
  #remove pediatric patients <16
  filter(REC_AGE_YEARS>=16) %>% 
  #create numeric last MELD value
  mutate(
    # extract numeric part from "MELD/PELD xx"
    CAN_LAST_SRTR_LAB_MELD_NUM = as.integer(
      str_extract(
        CAN_LAST_SRTR_LAB_MELD,
        regex("(?<=MELD/PELD)\\s*-?\\d+", ignore_case = TRUE)
      )
    ),
    # numeric last MELD (only where type == "M")
    MELD_NUM_Last = dplyr::if_else(
      CAN_LAST_SRTR_LAB_MELD_TY == "M",
      CAN_LAST_SRTR_LAB_MELD_NUM,
      as.integer(NA)
    ),
    # numeric last PELD (only where type == "P")
    PELD_NUM_Last = dplyr::if_else(
      CAN_LAST_SRTR_LAB_MELD_TY == "P",
      CAN_LAST_SRTR_LAB_MELD_NUM,
      as.integer(NA)
    ),
    CAN_INIT_SRTR_LAB_MELD_NUM = as.integer(
      str_extract(
        CAN_INIT_SRTR_LAB_MELD,
        regex("(?<=MELD/PELD)\\s*-?\\d+", ignore_case = TRUE)
      )
    ),
    # numeric initial MELD
    MELD_NUM_Init = dplyr::if_else(
      CAN_INIT_SRTR_LAB_MELD_TY == "M",
      CAN_INIT_SRTR_LAB_MELD_NUM,
      as.integer(NA)
    ),
    # numeric initial PELD
    PELD_NUM_Init = dplyr::if_else(
      CAN_INIT_SRTR_LAB_MELD_TY == "P",
      CAN_INIT_SRTR_LAB_MELD_NUM,
      as.integer(NA)
    )
  )%>% 
  #create binary for REC_FAILY_GRAFT_THROMB
  mutate(REC_FAIL_GRAFT_THROMB_BI=ifelse(REC_FAIL_GRAFT_THROMB=="Y", 1, 0)) %>% 
  mutate(REC_FAIL_GRAFT_THROMB_KI = na_if(as.character(REC_FAIL_GRAFT_THROMB), "")) %>% 
  mutate(REC_FAIL_GRAFT_THROMB_KI = case_when(
    REC_FAIL_GRAFT_THROMB == "Y" ~ "Y",
    REC_FAIL_GRAFT_THROMB == "N" ~ "N",
    REC_FAIL_GRAFT_THROMB %in% c("U", "", NA_character_, NA) ~ "U",
    TRUE ~ "U"
  )) %>%
  mutate(REC_FAIL_GRAFT_THROMB_KI = factor(REC_FAIL_GRAFT_THROMB_KI, levels = c("Y","N","U"))) %>% 
  #recode variable and create binary for REC_FAIL_INFECT for both ki and li
  mutate(REC_FAIL_INFECT_KI_recode = na_if(as.character(REC_FAIL_INFECT_KI), " ")) %>% 
  mutate(REC_FAIL_INFECT_KI_recode = case_when(
    REC_FAIL_INFECT_KI == "Y" ~ "Y",
    REC_FAIL_INFECT_KI == "N" ~ "N",
    REC_FAIL_INFECT_KI %in% c("U", "", NA_character_, NA) ~ "U",
    TRUE ~ "U"
  )) %>%
  mutate(REC_FAIL_INFECT_KI_recode = factor(REC_FAIL_INFECT_KI_recode, levels = c("Y","N","U"))) %>% 
  mutate(REC_FAIL_INFECT_KI_BI=ifelse(REC_FAIL_INFECT_KI=="Y", 1, 0)) %>% 
  #same for liver
  mutate(REC_FAIL_INFECT_LI_recode = na_if(as.character(REC_FAIL_INFECT_LI), " ")) %>%
  mutate(REC_FAIL_INFECT_LI_recode = case_when(
    REC_FAIL_INFECT_LI == "Y" ~ "Y",
    REC_FAIL_INFECT_LI == "N" ~ "N",
    REC_FAIL_INFECT_LI %in% c("U", "", NA_character_, NA) ~ "U",
    TRUE ~ "U"
  )) %>%
  mutate(REC_FAIL_INFECT_LI_recode = factor(REC_FAIL_INFECT_LI_recode, levels=c("Y","N","U"))) %>% 
  mutate(REC_FAIL_INFECT_LI_BI=ifelse(REC_FAIL_INFECT_LI=="Y", 1, 0)) %>% 
  #recode and create binary for REC_FAIL_RECUR_DISEASE for both ki and li
  mutate(
    REC_FAIL_RECUR_DISEASE_KI_recode = fct_na_value_to_level(REC_FAIL_RECUR_DISEASE_KI),
    REC_FAIL_RECUR_DISEASE_LI_recode = fct_na_value_to_level(REC_FAIL_RECUR_DISEASE_LI)
  ) %>%
  mutate(across(
    c(REC_FAIL_RECUR_DISEASE_KI_recode, REC_FAIL_RECUR_DISEASE_LI_recode), 
    ~ case_when(
      . == "Y" ~ "Y",
      . == "N" ~ "N",
      TRUE ~ "U"
    )
  )) %>%
  mutate(across(
    c(REC_FAIL_RECUR_DISEASE_KI_recode, REC_FAIL_RECUR_DISEASE_LI_recode), 
    ~ factor(., levels = c("Y","N","U"))
  )) %>% 
  mutate(REC_FAIL_RECUR_DISEASE_KI_BI=ifelse(REC_FAIL_RECUR_DISEASE_KI=="Y", 1, 0)) %>% 
  mutate(REC_FAIL_RECUR_DISEASE_LI_BI=ifelse(REC_FAIL_RECUR_DISEASE_LI=="Y", 1, 0)) %>% 
  #recode and create binary for REC_FAIL_REJ_ACUTE for both li and kidney 
  mutate(
    REC_FAIL_REJ_ACUTE_KI_recode = fct_na_value_to_level(REC_FAIL_REJ_ACUTE_KI),
    REC_FAIL_REJ_ACUTE_LI_recode = fct_na_value_to_level(REC_FAIL_REJ_ACUTE_LI)
  ) %>%
  mutate(across(
    c(REC_FAIL_REJ_ACUTE_KI_recode, REC_FAIL_REJ_ACUTE_LI_recode),
    ~ case_when(
      . == "Y" ~ "Y",
      . == "N" ~ "N",
      TRUE ~ "U"
    ),
    .names = "{.col}"  # keeps names exactly as written
  )) %>%
  mutate(across(
    c(REC_FAIL_REJ_ACUTE_KI_recode, REC_FAIL_REJ_ACUTE_LI_recode),
    ~ factor(., levels = c("Y","N","U"))
  )) %>% 
  mutate(REC_FAIL_REJ_ACUTE_KI_bi=ifelse(REC_FAIL_REJ_ACUTE_KI=="Y", 1, 0)) %>% 
  mutate(REC_FAIL_REJ_ACUTE_LI_bi=ifelse(REC_FAIL_REJ_ACUTE_LI=="Y", 1, 0)) %>% 
  #create binary for REC_FAIL_SURG_COMPL (only exists for kidney)
  mutate(REC_FAIL_SURG_COMPL_bi=ifelse(REC_FAIL_SURG_COMPL=="Y", 1, 0)) %>%  
  #create binary for REC_FAIL_UROL_COMPL (only exists for kidney)
  mutate(REC_FAIL_UROL_COMPL_bi=ifelse(REC_FAIL_UROL_COMPL=="Y", 1, 0)) %>%  
  #recode both urol comp and fail_surg complications just for kidney
  mutate(
    REC_FAIL_SURG_COMPL_recode = fct_na_value_to_level(REC_FAIL_SURG_COMPL),
    REC_FAIL_UROL_COMPL_recode = fct_na_value_to_level(REC_FAIL_UROL_COMPL)
  ) %>%
  mutate(across(
    c(REC_FAIL_SURG_COMPL_recode, REC_FAIL_UROL_COMPL_recode),
    ~ case_when(
      . == "Y" ~ "Y",
      . == "N" ~ "N",
      TRUE ~ "U"
    )
  )) %>%
  mutate(across(
    c(REC_FAIL_SURG_COMPL_recode, REC_FAIL_UROL_COMPL_recode),
    ~ factor(., levels = c("Y","N","U"))
  )) %>% 
  #recode recurrent hepatitis variable to 3 levels for liver only 
  mutate(
    REC_FAIL_HEP_RECUR_recode = case_when(
      REC_FAIL_HEP_RECUR == "Y" ~ "Y",
      REC_FAIL_HEP_RECUR == "N" ~ "N",
      TRUE ~ "U"   # includes U, "", NA, and anything unexpected
    ),
    REC_FAIL_HEP_RECUR_recode = factor(
      REC_FAIL_HEP_RECUR_recode,
      levels = c("Y", "N", "U")
    )
  ) %>% 
  #combine insurance type for kidney and liver, and then categorize as private vs public vs other/na
  mutate(
    REC_PRIMARY_PAY = case_when(
      !is.na(REC_PRIMARY_PAY_LI) ~ REC_PRIMARY_PAY_LI,                             
      is.na(REC_PRIMARY_PAY_LI) & !is.na(REC_PRIMARY_PAY_KI) ~ REC_PRIMARY_PAY_KI,       
      TRUE ~ NA_character_                                          
    ) 
  )%>% 
  mutate(
    REC_PRIMARY_PAY = case_when(
      REC_PRIMARY_PAY %in% c("1: Private insurance") ~ "Private",
      
      REC_PRIMARY_PAY %in% c(
        "2: Public insurance - Medicaid",
        "3: Public insurance - Medicare FFS (Fee for Service)",
        "4: Public insurance - Medicare & Choice",
        "5: Public insurance - CHIP (Children's Health Insurance Program)",
        "6: Public insurance - Department of VA",
        "7: Public insurance - Other government",
        "13: Public insurance - Medicare Unspecified",
        "14: US/State Govt Agency"
      ) ~ "Public",
      
      REC_PRIMARY_PAY %in% c(
        "8: Self",
        "9: Donation",
        "10: Free Care",
        "11: Pending",
        "12: Foreign Government Specify",
        "15: Unknown"
      ) ~ "Other/NA",
      
      TRUE ~ "Other/NA"
    )
  ) %>% 
  #recode and create binary variable for REC_FAIL_HEP_denovo (cause of graft failure recurrent hepatitis)
  mutate(REC_FAIL_HEP_DENOVO_bi=ifelse(REC_FAIL_HEP_DENOVO=="Y", 1, 0)) %>%
  mutate(
    REC_FAIL_HEP_DENOVO_recode =
      case_when(
        REC_FAIL_HEP_DENOVO == "Y" ~ "Y",
        REC_FAIL_HEP_DENOVO == "N" ~ "N",
        TRUE ~ "U"   # NA, "", "U", Missing all → "U"
      ),
    REC_FAIL_HEP_DENOVO_recode = factor(REC_FAIL_HEP_DENOVO_recode, levels = c("Y", "N", "U"))
  ) %>% 
  #recode and also create binary variable for REC_FAIL_VASC_THROMB
  mutate(REC_FAIL_VASC_THROMB_bi=ifelse(REC_FAIL_VASC_THROMB=="Y", 1, 0)) %>% 
  mutate(
    REC_FAIL_VASC_THROMB_recode =
      case_when(
        REC_FAIL_VASC_THROMB == "Y" ~ "Y",
        REC_FAIL_VASC_THROMB == "N" ~ "N",
        TRUE ~ "U"
      ),
    REC_FAIL_VASC_THROMB_recode = factor(REC_FAIL_VASC_THROMB_recode, levels = c("Y", "N", "U"))
  ) %>% 
  #combine REC_TX_ORG_TY
  mutate(
    REC_TX_ORG_TY = case_when(
      !is.na(REC_TX_ORG_TY_LI) ~ REC_TX_ORG_TY_LI,                             
      is.na(REC_TX_ORG_TY_LI) & !is.na(REC_TX_ORG_TY_KI) ~ REC_TX_ORG_TY_KI,       
      TRUE ~ NA_character_                                          
    ) 
  )%>%
  #exclude multi-organ transplants (K_L_I and K_L_H)
  filter(!REC_TX_ORG_TY %in% c("KI LI IN: Kidney-Liver-Intestine", "KI LI HR: Kidney-Liver-Heart")) %>% 
  #combine rec BMI for liver and kidney data
  mutate(
    REC_BMI=coalesce(REC_BMI_KI,REC_BMI_KI)
  ) %>%
  #combine candidate race
  mutate(
    CAN_RACE = case_when(
      !is.na(CAN_RACE_LI) ~ CAN_RACE_LI,
      is.na(CAN_RACE_LI) & !is.na(CAN_RACE_KI) ~ CAN_RACE_KI,
      TRUE ~ NA_character_
    )
  )%>%
  #combine candidate ethnicity
  mutate(
    CAN_ETHNICITY_SRTR = case_when(
      !is.na(CAN_ETHNICITY_SRTR_LI) ~ CAN_ETHNICITY_SRTR_LI,
      is.na(CAN_ETHNICITY_SRTR_LI) & !is.na(CAN_ETHNICITY_SRTR_KI) ~ CAN_ETHNICITY_SRTR_KI,
      TRUE ~ NA_character_
    )
  ) %>%
  #combine and recode missing for rec_malignancy_type
  mutate(
    # Step 1: Clean raw inputs ("" → NA)
    REC_MALIG_TY_LI_clean = na_if(as.character(REC_MALIG_TY_LI), ""),
    REC_MALIG_TY_KI_clean = na_if(as.character(REC_MALIG_TY_KI), ""),
    
    # Step 2: Convert literal "Missing" into NA
    REC_MALIG_TY_LI_clean = if_else(
      REC_MALIG_TY_LI_clean == "Missing", NA_character_, REC_MALIG_TY_LI_clean
    ),
    REC_MALIG_TY_KI_clean = if_else(
      REC_MALIG_TY_KI_clean == "Missing", NA_character_, REC_MALIG_TY_KI_clean
    ),
    
    # Step 3: Prefer whichever actually has data;
    # if both have data, prefer liver; if neither, "Missing"
    REC_MALIG_TY_COMBINED = case_when(
      !is.na(REC_MALIG_TY_LI_clean) ~ REC_MALIG_TY_LI_clean,  # liver first IF it's real
      is.na(REC_MALIG_TY_LI_clean) & !is.na(REC_MALIG_TY_KI_clean) ~ REC_MALIG_TY_KI_clean,  # else kidney
      TRUE ~ "Missing"  # neither has data
    ),
    
    # Step 4: Turn into factor (optional)
    REC_MALIG_TY_COMBINED = factor(REC_MALIG_TY_COMBINED)
  ) %>%
  select(-REC_MALIG_TY_LI_clean, -REC_MALIG_TY_KI_clean) %>% 
  #recode can_last_Ascites and can_last_enceph to combine missing with unknown 
  mutate(
    CAN_LAST_ASCITES = factor(
      dplyr::case_when(
        CAN_LAST_ASCITES %in% c("4: N/A", "Unknown", NA, "") ~ "Unknown",
        TRUE ~ CAN_LAST_ASCITES
      ),
      levels = c("1: Absent", "2: Slight", "3: Moderate", "Unknown")
    ),
    
    CAN_LAST_ENCEPH = factor(
      dplyr::case_when(
        CAN_LAST_ENCEPH %in% c("4: N/A", "Unknown", NA, "") ~ "Unknown",
        TRUE ~ CAN_LAST_ENCEPH
      ),
      levels = c("1: None", "2: 1-2", "3: 3-4", "Unknown")
    )
  ) %>% 
  #create length of hospital stay after transplant variable, exclude those who died prior to d/c
  mutate(
    REC_DISCHRG_DT  = coalesce(REC_DISCHRG_DT_LI, REC_DISCHRG_DT_KI),
    
    los_days = as.integer(REC_DISCHRG_DT - REC_TX_DT),
    los_days = if_else(REC_DEATH_BINARY == 1 & 
                         REC_DEATH_DT_COMPOSITE <= REC_DISCHRG_DT, NA_integer_, los_days),
    
    # If you want “day of transplant = day 1”
    los_days_inclusive = if_else(!is.na(los_days), los_days + 1L, NA_integer_)
  ) %>% 
  #collapse race variable into smaller categories
  mutate(
    # Extract the leading numeric code if present (e.g., "16" from "16: Black...")
    CAN_RACE_code = as.integer(str_extract(as.character(CAN_RACE), "^[0-9]+")),
    
    # Collapsed race variable
    CAN_RACE_collapsed = case_when(
      is.na(CAN_RACE) ~ "Missing",
      
      CAN_RACE_code == 8    ~ "White",
      CAN_RACE_code == 16   ~ "Black",
      CAN_RACE_code == 2000 ~ "Hispanic/Latino",
      CAN_RACE_code %in% c(64, 32, 128) ~ "Other",
      
      TRUE ~ "Other"
    ) %>%
      factor(levels = c("White", "Black", "Hispanic/Latino", "Other", "Missing"))
  )


# ----how many die before discharge?----
death_num_before_dc<-sum(!is.na(tx_slk_final$REC_DEATH_DT_COMPOSITE) & tx_slk_final$REC_DEATH_DT_COMPOSITE <= tx_slk_final$REC_DISCHRG_DT, na.rm = TRUE) #448


# ----keep original labels----
var_label(tx_slk_final$CAN_RACE)<-var_label(tx_slk_final$CAN_RACE_LI)
var_label(tx_slk_final$CAN_ETHNICITY_SRTR)<-var_label(tx_slk_final$CAN_ETHNICITY_SRTR_LI)
var_label(tx_slk_final$REC_FAIL_VASC_THROMB_recode)<-var_label(tx_slk_final$REC_FAIL_VASC_THROMB)
var_label(tx_slk_final$REC_FAIL_HEP_DENOVO_recode)<-var_label(tx_slk_final$REC_FAIL_HEP_DENOVO)



# ----Break down era----
table(tx_slk_final$ERA, useNA = "ifany")
table(tx_slk_final$ERA, tx_slk_final$HIV_POSITIVE, useNA = "ifany")
summary(tx_slk_final$YEAR_TRANSPLANT)
table(tx_slk_final$TRANSPLANT_REASON_LI)


multi_kidney_organs <- tx_slk_final %>%
  filter(REC_TX_ORG_TY_KI %in% c("KI LI IN: Kidney-Liver-Intestine", "KI LI HR: Kidney-Liver-Heart"))  # adjust names if UNOS uses different labels


# Count HIV status in this group
table(multi_kidney_organs$HIV_POSITIVE, useNA = "ifany")

# Number of HIV positive recipients
sum(multi_kidney_organs$REC_HIV_STAT_KI == "P", na.rm = TRUE)


table(tx_slk_final$DON_DEATH_MECH)



summary(tx_slk_final$REC_DEATH_yrs)

# Should be >= 0
sum(tx_slk_final$REC_DEATH_yrs < 0, na.rm = TRUE)
if (sum(tx_slk_final$REC_DEATH_yrs < 0, na.rm = TRUE) > 0) {
  stop("REC_DEATH_yrs contains negative values")
}

# ----Check short follow-up----
summary(tx_slk_final$REC_DEATH_yrs[tx_slk_final$REC_DEATH_BINARY == 1])

#Source of information that someone died
tx_slk_final %>%
  filter(
    !is.na(PERS_OPTN_DEATH_DT_LI) | !is.na(PERS_OPTN_DEATH_DT_KI)
  ) %>%
  count(REC_DEATH_SOURCE)

# ----Are LI and KI transplant dates basically the same?----
summary(as.numeric(difftime(tx_slk_final$REC_TX_DT_LI, tx_slk_final$REC_TX_DT_KI, units="days")))


# ----figure out which donor repeats----
dup_donors <- tx_slk_final$DONOR_ID_LI[duplicated(tx_slk_final$DONOR_ID_LI)]
dup_donors
repeat_donor_id <- dup_donors[1]   # get the ID
repeat_donor_cases <- tx_slk_final %>%
  filter(DONOR_ID_LI == repeat_donor_id)
repeat_donor_cases


