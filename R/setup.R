#Setup

# ----This section imports libraries necessary for the analysis----

#File import libraries
library(haven)
library(readr)
library(readxl)

#File export libraries
library(writexl)

#Data management libraries
library(tidyverse)
library(labelled)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)
library(lubridate)
library(conflicted)

#Core library for the SRTR
library(sRtr)

#Flowchart libraries
library(strobe)
library(flowchart)

#Statistical modeling libraries
library(MatchIt)
library(survival)
library(MASS)
library(sandwich)
library(lmtest)


#Table libraries
library(gtsummary)
library(gt)

#Plotting libraries
library(ggplot2)
library(survminer)

#Use the conflicted package to ensure that the select and filter functions are not superseded
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")



#----Set variable labels for tables----
var_labels <- c(
  
  # Donor variables
  DON_AGE = "Donor age (years)",
  DON_GENDER = "Donor sex",
  DON_ETHNICITY_SRTR = "Donor ethnicity",
  DON_RACE = "Donor race",
  DON_EXPAND_DON_KI = "Expanded criteria donor (kidney)",
  DON_DEATH_MECH_CAT = "Donor mechanism of death",
  DON_HIV_NAT_RECODED = "Donor HIV NAT",
  DON_ANTI_HIV_recode = "Donor anti-HIV antibody",
  DON_HCV_STAT_RECODED = "Donor HCV status",
  DON_ANTI_HCV_RECODED = "Donor anti-HCV antibody",
  DON_HBC_STAT_RECODED = "Donor hepatitis B core antibody",
  
  # Recipient – Liver
  REC_AGE_YEARS = "Recipient age (years)",
  REC_AGE_AT_TX = "Recipient age at transplant (years)",
  REC_COLD_ISCH_TM_LI = "Liver cold ischemia time (hours)",
  TRANSPLANT_REASON_LI = "Primary liver diagnosis",
  REC_FAIL_HEP_DENOVO_recode = "De novo hepatitis after transplant",
  REC_FAIL_HEP_RECUR_recode = "Recurrent hepatitis after transplant",
  REC_FAIL_VASC_THROMB_recode = "Liver graft failure due to vascular thrombosis",
  REC_FAIL_INFECT_LI_recode = "Liver graft failure due to infection",
  REC_FAIL_RECUR_DISEASE_LI_recode = "Liver graft failure due to recurrent disease",
  REC_FAIL_REJ_ACUTE_LI_recode = "Liver graft failure due to acute rejection",
  REC_ACUTE_REJ_EPISODE_LI = "Acute liver rejection before discharge",
  TFL_GRAFT_DT_LI_yrs = "Time to liver graft failure (years)",
  REC_DEATH_yrs = "Time to death (years)",
  
  # Recipient – Kidney
  REC_ACUTE_REJ_EPISODE_KI = "Acute kidney rejection before discharge",
  REC_COLD_ISCH_TM_KI = "Kidney cold ischemia time (hours)",
  TRANSPLANT_REASON_KI = "Primary kidney diagnosis",
  REC_FAIL_GRAFT_THROMB_KI = "Kidney graft failure due to thrombosis",
  REC_FAIL_INFECT_KI_recode = "Kidney graft failure due to infection",
  REC_FAIL_RECUR_DISEASE_KI_recode = "Kidney graft failure due to recurrent disease",
  REC_FAIL_REJ_ACUTE_KI_recode = "Kidney graft failure due to acute rejection",
  REC_FAIL_SURG_COMPL_recode = "Kidney graft failure due to surgical complication",
  REC_FAIL_UROL_COMPL_recode = "Kidney graft failure due to urologic complication",
  REC_HBV_ANTIBODY_COMB = "Recipient hepatitis B antibody",
  REC_HBV_SURF_ANTIGEN_COMB = "Recipient hepatitis B surface antigen",
  REC_HCV_STAT_COMB = "Recipient hepatitis C status",
  REC_CMV_COMBINED = "Recipient CMV status",
  REC_MALIG_TY_HEPCARCINOMA_COMB = "History of hepatocellular carcinoma",
  REC_MALIG_TY_LIVER_COMB = "History of other liver malignancy",
  TFL_GRAFT_DT_KI_yrs = "Time to kidney graft failure (years)",
  
  # Candidate – Liver
  CAN_PORTAL_VEIN_recode = "Portal vein thrombosis",
  CAN_TIPSS_recode = "Prior TIPS",
  CAN_ASCITES_recode = "Ascites at listing",
  CAN_BACTERIA_PERIT_recode = "History of bacterial peritonitis",
  CAN_ENCEPH_recode = "Hepatic encephalopathy at listing",
  CAN_LAST_ASCITES = "Ascites at transplant",
  CAN_LAST_ALBUMIN = "Serum albumin at transplant (g/dL)",
  CAN_LAST_BILI = "Total bilirubin at transplant (mg/dL)",
  CAN_LAST_INR = "INR at transplant",
  CAN_LAST_SERUM_CREAT = "Serum creatinine at transplant (mg/dL)",
  CAN_LAST_SERUM_SODIUM = "Serum sodium at transplant (mEq/L)",
  CAN_LAST_ENCEPH = "Encephalopathy at transplant",
  MELD_NUM_Init = "Initial MELD score",
  MELD_NUM_Last = "MELD score at transplant",
  DON_TY = "Donor type",
  median_waitlist_time_years_li = "Median liver waitlist time (years)",
  
  # Candidate – Kidney
  CAN_AGE_AT_LISTING_YR_KI = "Age at kidney listing (years)",
  CAN_GENDER = "Recipient sex",
  CAN_ETHNICITY_SRTR = "Recipient ethnicity",
  CAN_RACE = "Recipient race",
  CAN_RACE_collapsed = "Recipient race collapsed",
  CAN_BMI_clean = "Body mass index (kg/m²)",
  CAN_CREAT_CLEAR = "Creatinine clearance",
  CAN_DIAB = "Diabetes mellitus",
  CAN_DIAL = "On dialysis at listing",
  TIME_ON_DIALYSIS_YEARS = "Time on dialysis (years)",
  CAN_DRUG_TREAT_HYPERTEN_COMB = "Treated hypertension",
  CAN_FUNCTN_STAT_clean = "Functional status",
  CAN_GFR = "Glomerular filtration rate",
  CAN_MALIG_COMB = "History of malignancy",
  median_waitlist_time_years_ki = "Median kidney waitlist time (years)"
)
