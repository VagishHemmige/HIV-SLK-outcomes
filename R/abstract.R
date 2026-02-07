#Let's analyze data for January 5th Abstract



#Exposure: HIV_POSITIVE
#Outcome: Overall Survival
#Matching: Age, Gender, Era, MELD,HCV status?
#Matching ratio, 1:5
#Primary Analysis: Matched KM and Cox 

#restrict sample set to after 2005
tx_slk_2005 <- tx_slk_final %>%
  filter(YEAR_TRANSPLANT >= 2005)

#create smaller dataset 
vars_needed <- c(
  "HIV_POSITIVE", "ERA",
  "REC_AGE_YEARS", "CAN_GENDER", "REC_HCV_STAT_COMB",
  "TRANSPLANT_REASON_LI", "MELD_NUM_Last",
  "REC_DEATH_yrs", "REC_DEATH_BINARY", "PERS_ID", "DONOR_ID", "REC_TX_DT_LI"
)
slk_small <- tx_slk_2005 %>%
  select(any_of(vars_needed)) %>%
  filter(complete.cases(.))

#identify all HIV positive cases:
hiv_pos_all <- tx_slk_2005 %>%
  select(any_of(vars_needed)) %>%
  filter(HIV_POSITIVE == 1)
nrow(hiv_pos_all) #56
#identify complete HIV cases
hiv_pos_complete <- tx_slk_2005 %>%
  select(any_of(vars_needed)) %>%
  filter(HIV_POSITIVE == 1) %>%
  filter(complete.cases(.))
nrow(hiv_pos_complete)
#identify lost cases:
lost_hiv_pos <- anti_join(
  hiv_pos_all,
  hiv_pos_complete,
  by = "PERS_ID"
)
nrow(lost_hiv_pos)
#why missing?
missing_summary <- lost_hiv_pos %>%
  summarise(across(
    all_of(vars_needed),
    ~ sum(is.na(.x))
  )) %>%
  pivot_longer(
    everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  arrange(desc(n_missing))

missing_summary #lost cases to missing transplant reason

#save lost HIV cases
saveRDS(lost_hiv_pos, "~/Desktop/slk_match/lost_hiv_positive_cases.rds")


#used 2014 as DAA era cut off as per paper
#1:5 matching feasible
#match exact on ERA
#primary outcome = survival (can consider graft failure but keep simple for abstract)

Matching_Variable_List<-c("REC_AGE_YEARS",
                          "CAN_GENDER",
                          "REC_HCV_STAT_COMB",
                          "TRANSPLANT_REASON_LI",
                          "MELD_NUM_Last",
                          "ERA"
                          )

#save small dataset
saveRDS(slk_small, "~/Desktop/slk_match/slk_small_for_matching.rds")
#define matching formula, run in base R
match_formula <- HIV_POSITIVE ~ 
  REC_AGE_YEARS + CAN_GENDER + REC_HCV_STAT_COMB +
  TRANSPLANT_REASON_LI + MELD_NUM_Last
# m2 <- matchit(
#   formula = match_formula,
#   data    = slk_small,
#   exact   = ~ ERA,
#   method  = "nearest",
#   ratio   = 5
# )
#for future -- keep in mind that AGE did not match that well 0.21
#read in match from baseR 
m2 <- readRDS("~/Desktop/slk_match/m2_nearest_ratio5.rds")
matched_df5 <- match.data(m2)

summary(m2)
with(matched_df5, table(ERA, HIV_POSITIVE))

# check size
nrow(matched_df5)
with(matched_df5, table(ERA, HIV_POSITIVE))


saveRDS(matched_df5, "~/Desktop/slk_match/slk_matched_nearest_ratio5.rds")

#Survival Analysis
cox_main <- coxph(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE,
  data = matched_df5
)
summary(cox_main)



km_fit <- survfit(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE,
  data = matched_df5
)

summary(km_fit)
logrank <- survdiff(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE,
  data = matched_df5
)

logrank


#cox model stratified by era
cox_era <- coxph(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE + strata(ERA),
  data = matched_df5
)

summary(cox_era)

#cox model stratified by MELD era and DAA era
matched_df5 <- matched_df5 %>%
  mutate(
    TX_DATE = as.Date(REC_TX_DT_LI),   # use your tx date var
    MELD_ERA = case_when(
      TX_DATE < as.Date("2016-01-11") ~ "Pre-MELDNa",
      TRUE ~ "MELDNa"
    )
  )

#sensitivity analysis -- stratification by MELD era, definition change in 2016:
cox_both <- coxph(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE + strata(ERA) + strata(MELD_ERA),
  data = matched_df5
)

summary(cox_both)
with(matched_df5, table(as.integer(format(as.Date(REC_TX_DT_LI), "%Y")) < 2016, HIV_POSITIVE))

#sensitivity analysis -- adjusting for MELD era 
cox_meldEra_adj <- coxph(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE + strata(ERA) + MELD_ERA,
  data = matched_df5
)
summary(cox_meldEra_adj)



#making tables
Cox_table_slk<-coxph(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, 
                     data = matched_df5)%>%
  tbl_regression(exponentiate=TRUE)
Cox_table_slk

Cox_table_slk_strat<-coxph(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE + strata(ERA), 
                     data = matched_df5)%>%
  tbl_regression(exponentiate=TRUE)
Cox_table_slk_strat

#make plot
slk_death_plot <- ggsurvplot(
  km_fit,
  data = matched_df5,
  risk.table = TRUE,
  pval = TRUE,
  pval.coord = c(1, 0.24),
  conf.int = TRUE,
  break.time.by = 1,
  title = "Survival of HIV positive and HIV negative SLK transplant recipients",
  xlab = "Time post-transplant (years)",
  ylab = "Proportion surviving",
  legend.labs = c("HIV negative", "HIV positive"),
  risk.table.height = 0.3,
  xlim = c(0, 7),
  ylim = c(0, 1)
)

print(slk_death_plot)
ggsave(
  filename = "SLK_HIV_survival_KM.pdf",
  plot = slk_death_plot$plot,
  width = 7,
  height = 6
)


#merge in variables to make demographic tables 
intersect(names(matched_df5), names(tx_slk_final))

matched_full <- tx_slk_final %>%
  semi_join(
    matched_df5 %>% select(PERS_ID),
    by = "PERS_ID"
  )


# Check row count
nrow(matched_full)
nrow(matched_df5)

# Check HIV balance
with(matched_full, table(ERA, HIV_POSITIVE))

#make demographic variables 
matched_full %>%
  select(
    HIV_POSITIVE,
    all_of(Don_Var_List),
    all_of(Rec_Li_Var_List),
    all_of(Rec_Ki_Var_List),
    all_of(Cand_Li_Var_List),
    all_of(Cand_Ki_Var_List)
  ) %>%
  tbl_summary(
    by = HIV_POSITIVE,
    missing = "ifany",
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_p() %>% 
  as_gt() %>%
  gt::cols_label(
    stat_1 = "Negative/Unknown",
    stat_2 = "Positive"
  ) %>%
  gt::tab_header(
    title = "Characteristics of HIV positive and negative SLK recipients"
  ) %>% 
  gt::gtsave("matched_table.png")

table(tx_slk_final$HIV_POSITIVE)

###PRE AND POST DAA Separate ANALYSIS
# sanity checks
table(matched_df5$ERA, useNA = "ifany")
table(matched_df5$HIV_POSITIVE, useNA = "ifany")

# make sure ERA is exactly "Pre-DAA" / "Post-DAA"
unique(matched_df5$ERA)


#create 2 separate era specific datasets
pre_df  <- subset(matched_df5, ERA == "Pre-DAA")
post_df <- subset(matched_df5, ERA == "Post-DAA")

# quick check counts and events
with(pre_df,  table(HIV_POSITIVE))
with(post_df, table(HIV_POSITIVE))

sum(pre_df$REC_DEATH_BINARY == 1, na.rm = TRUE)
sum(post_df$REC_DEATH_BINARY == 1, na.rm = TRUE)

#run KM and Log rank separately 
library(survival)

# Pre-DAA
km_pre <- survfit(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = pre_df)
lr_pre <- survdiff(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = pre_df)

# Post-DAA
km_post <- survfit(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = post_df)
lr_post <- survdiff(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = post_df)

lr_pre
lr_post

#run cox separately by era
cox_pre  <- coxph(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = pre_df)
cox_post <- coxph(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = post_df)

summary(cox_pre)
summary(cox_post)

#interaction model
matched_df5$ERA <- factor(matched_df5$ERA)  # ensure factor

cox_int <- coxph(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE * ERA,
  data = matched_df5
)

summary(cox_int)


#KM Plots with separation by Era
install.packages("survminer")  # only if you haven't
library(survminer)

library(survminer)

# Pre-DAA KM with risk table
p_pre<-ggsurvplot(
  km_pre,
  data = pre_df,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Years post-transplant",
  ylab = "Survival probability",
  legend.labs = c("HIV negative", "HIV positive"),
  title = "Overall survival after SLK (Pre-DAA era)"
)

# Save KM curve + risk table together
ggsave(
  filename = "KM_PreDAA.png",
  plot     = p_pre$combined,
  width    = 8,
  height   = 7,
  dpi      = 300
)

# Post-DAA KM with risk table
p_post<-ggsurvplot(
  km_post,
  data = post_df,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Years post-transplant",
  ylab = "Survival probability",
  legend.labs = c("HIV negative", "HIV positive"),
  title = "Overall survival after SLK (Post-DAA era)"
)

ggsave(
  filename = "KM_PostDAA.png",
  plot     = p_post$combined,
  width    = 8,
  height   = 7,
  dpi      = 300
)
print(p_pre)
print(p_post)


#estimated median survival
#overall curve
km_all <- survfit(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE,
                  data = matched_df5)

summary(km_all)$table

#pre DAA
km_pre <- survfit(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE,
                  data = pre_df)

summary(km_pre)$table

#post DAA
km_post <- survfit(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE,
                   data = post_df)

summary(km_post)$table


# choose timepoints in YEARS because your time variable is REC_DEATH_yrs
summary(km_post, times = c(1, 3, 5))$surv
summary(km_post, times = c(1, 3, 5))$std.err
s <- summary(km_post, times = c(1,3,5))
data.frame(
  strata = s$strata,
  time_years = s$time,
  surv = s$surv
)

summary(matched_df5$REC_AGE_YEARS[matched_df5$HIV_POSITIVE])
summary(matched_df5$REC_AGE_YEARS)

median(
  matched_df5$REC_AGE_YEARS[matched_df5$HIV_POSITIVE == 1],
  na.rm = TRUE
)


df <- matched_df5  # or tx_slk_final if you want full cohort

df <- df %>%
  mutate(
    HCV_pos = REC_HCV_STAT_COMB %in% c("P", "Positive", "Y", "Yes", 1)
  )
hcv_by_era_hiv <- df %>%
  group_by(ERA, HIV_POSITIVE) %>%
  summarise(
    n = n(),
    n_hcv = sum(HCV_pos, na.rm = TRUE),
    pct_hcv = 100 * mean(HCV_pos, na.rm = TRUE),
    .groups = "drop"
  )

hcv_by_era_hiv
with(matched_df5, table(ERA, HIV_POSITIVE, REC_HCV_STAT_COMB, useNA = "ifany"))


#formal test if effect of HIV on mortality differs between eras

#use this one
fit_interaction_matched_data <- coxph(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE * ERA,
  data = matched_df5
)

summary(fit_interaction_matched_data) #p<.01

