# Cox regression for overall mortality, separated by DAA era


# If weights exist and are all 1 (your case), you can ignore them.
# But we'll code it safely to auto-handle either situation:
wts <- if ("weights" %in% names(matched_df)) matched_df$weights else NULL

matched_df$ERA <- factor(matched_df$ERA, levels = c("Pre-DAA", "Post-DAA"))


#cox models
cox_main <- coxph(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE,
  data    = matched_df,
  robust  = TRUE,
  cluster = subclass
)
summary(cox_main)

# #adjusted for DAA era
# cox_main_effects <- coxph(
#   Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE + ERA,
#   data    = matched_df,
#   robust  = TRUE,
#   cluster = subclass
# )
# summary(cox_main_effects)
# 
#formally test if ERA is effect modifier
cox_int <- coxph(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE * ERA,
  data    = matched_df,
  robust  = TRUE,
  cluster = subclass
)
summary(cox_int)


#split analysis pre and post DAA era

# Split datasets
pre_df  <- matched_df %>% filter(ERA == "Pre-DAA")
post_df <- matched_df %>% filter(ERA == "Post-DAA")

# Optional: check counts/events
with(pre_df,  table(HIV_POSITIVE))
with(post_df, table(HIV_POSITIVE))
sum(pre_df$REC_DEATH_BINARY == 1, na.rm = TRUE)
sum(post_df$REC_DEATH_BINARY == 1, na.rm = TRUE)

# 1) Unadjusted (HIV only)
cox_pre_unadj <- coxph(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE,
  data    = pre_df,
  robust  = TRUE,
  cluster = subclass
)

cox_post_unadj <- coxph(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE,
  data    = post_df,
  robust  = TRUE,
  cluster = subclass
)

summary(cox_pre_unadj)
summary(cox_post_unadj)

#make these into tables

#make these into tables
# helper to make a tbl_regression with consistent formatting
mk_tbl <- function(fit) {
  tbl_regression(
    fit,
    exponentiate = TRUE
  ) %>%
    bold_labels() %>%
    italicize_levels()
}

t_unadj <- mk_tbl(cox_main)
t_era   <- mk_tbl(cox_main_effects)
t_int   <- mk_tbl(cox_int)
t_pre   <- mk_tbl(cox_pre_unadj)
t_post  <- mk_tbl(cox_post_unadj)


#helper function to make clean labels
clean_labels <- function(tbl) {
  tbl %>%
    modify_table_body(~ .x %>%
                        mutate(
                          label = case_when(
                            label == "HIV_POSITIVE" ~ "HIV-positive (vs HIV-negative)",
                            label == "ERAPost-DAA" ~ "Post-DAA era (vs Pre-DAA)",
                            label == "HIV_POSITIVE:ERAPost-DAA" ~ "HIV × Post-DAA interaction",
                            TRUE ~ label
                          )
                        )
    )
}

t_unadj <- clean_labels(t_unadj)
t_era   <- clean_labels(t_era)
t_int   <- clean_labels(t_int)
t_pre   <- clean_labels(t_pre)
t_post  <- clean_labels(t_post)


tbl_mortality <- tbl_merge(
  tbls = list(t_unadj, t_era, t_int, t_pre, t_post),
  tab_spanner = c(
    "**Unadjusted**",
    "**Adjusted for ERA**",
    "**HIV × ERA**",
    "**Pre-DAA only**",
    "**Post-DAA only**"
  )
) %>%
  modify_caption("**Table 1. Cox proportional hazards models for overall mortality**")

tbl_mortality



tbl_mortality_long <- tbl_stack(
  tbls = list(t_unadj, t_era, t_int, t_pre, t_post),
  group_header = c(
    "Unadjusted",
    "Adjusted for ERA",
    "HIV × ERA interaction",
    "Pre-DAA only",
    "Post-DAA only"
  )
) %>%
  modify_caption("**Table 1. Cox proportional hazards models for overall mortality**")

tbl_mortality_long



era_mixing <- matched_df %>%
  dplyr::group_by(subclass) %>%
  dplyr::summarise(
    n = dplyr::n(),
    eras = dplyr::n_distinct(ERA),
    mixed_era = (eras > 1)
  )

table(era_mixing$mixed_era)
mean(era_mixing$mixed_era)



matched_df %>%
  dplyr::mutate(TX_DATE = as.Date(REC_TX_DT)) %>%
  dplyr::group_by(subclass) %>%
  dplyr::summarise(
    min_date = min(TX_DATE, na.rm = TRUE),
    max_date = max(TX_DATE, na.rm = TRUE),
    span_days = as.numeric(max_date - min_date),
    eras = dplyr::n_distinct(ERA),
    mixed_era = eras > 1
  ) %>%
  dplyr::summarise(
    max_span = max(span_days, na.rm = TRUE),
    pct_mixed = mean(mixed_era, na.rm = TRUE)
  )



#Secondary Outcome Analysis: Graft Failure for Liver and Kidney 
#Redefining the censor date to include death as a censor (not just last f/u)
matched_df <- matched_df %>%
  mutate(
    # Ensure Date class
    REC_TX_DT_LI = as.Date(REC_TX_DT_LI),
    REC_TX_DT_KI = as.Date(REC_TX_DT_KI),
    TFL_LAFUDATE_LI = as.Date(TFL_LAFUDATE_LI),
    TFL_LAFUDATE_KI = as.Date(TFL_LAFUDATE_KI),
    TFL_GRAFT_DT_LI = as.Date(TFL_GRAFT_DT_LI),
    TFL_GRAFT_DT_KI = as.Date(TFL_GRAFT_DT_KI),
    REC_DEATH_DT_COMPOSITE = as.Date(REC_DEATH_DT_COMPOSITE),
    
    # LIVER: death-censored endpoint
    # censor date = earliest of (graft failure, death, last follow-up)
    LI_CENSOR_DT = pmin(TFL_GRAFT_DT_LI, REC_DEATH_DT_COMPOSITE, TFL_LAFUDATE_LI, na.rm = TRUE),
    LI_EVENT = ifelse(!is.na(TFL_GRAFT_DT_LI) & TFL_GRAFT_DT_LI <= LI_CENSOR_DT, 1, 0),
    LI_TIME_YRS = as.numeric(difftime(LI_CENSOR_DT, REC_TX_DT_LI, units = "days")) / 365.25,
    
    # KIDNEY: death-censored endpoint
    KI_CENSOR_DT = pmin(TFL_GRAFT_DT_KI, REC_DEATH_DT_COMPOSITE, TFL_LAFUDATE_KI, na.rm = TRUE),
    KI_EVENT = ifelse(!is.na(TFL_GRAFT_DT_KI) & TFL_GRAFT_DT_KI <= KI_CENSOR_DT, 1, 0),
    KI_TIME_YRS = as.numeric(difftime(KI_CENSOR_DT, REC_TX_DT_KI, units = "days")) / 365.25
  )

#Kidney graft failure
cox_kidney <- coxph(
  Surv(KI_TIME_YRS, KI_EVENT) ~ HIV_POSITIVE,
  data    = matched_df,
  robust  = TRUE,
  cluster = subclass
)
summary(cox_kidney)
cox.zph(cox_kidney)


#Liver graft failure
cox_liver <- coxph(
  Surv(LI_TIME_YRS, LI_EVENT) ~ HIV_POSITIVE,
  data    = matched_df,
  robust  = TRUE,
  cluster = subclass
)
summary(cox_liver)
cox.zph(cox_liver)

# #Kidney + Era
# cox_kidney_era <- coxph(
#   Surv(KI_TIME_YRS, KI_EVENT) ~ HIV_POSITIVE + ERA,
#   data    = matched_df,
#   robust  = TRUE,
#   cluster = subclass
# )
# summary(cox_kidney_era)
# 
#Kidney * Era (interaction model)
cox_kidney_int <- coxph(
  Surv(KI_TIME_YRS, KI_EVENT) ~ HIV_POSITIVE * ERA,
  data    = matched_df,
  robust  = TRUE,
  cluster = subclass
)
summary(cox_kidney_int)
# 
# #Liver + Era model
# cox_liver_era <- coxph(
#   Surv(LI_TIME_YRS, LI_EVENT) ~ HIV_POSITIVE + ERA,
#   data    = matched_df,
#   robust  = TRUE,
#   cluster = subclass
# )
# summary(cox_liver_era)
# 
#Liver*Era interaction model
cox_liver_int <- coxph(
  Surv(LI_TIME_YRS, LI_EVENT) ~ HIV_POSITIVE * ERA,
  data    = matched_df,
  robust  = TRUE,
  cluster = subclass
)
summary(cox_liver_int)

#analysis stratified by era
pre_df  <- matched_df %>% filter(ERA == "Pre-DAA")
post_df <- matched_df %>% filter(ERA == "Post-DAA")

cox_kidney_pre <- coxph(Surv(KI_TIME_YRS, KI_EVENT) ~ HIV_POSITIVE,
                        data = pre_df, robust = TRUE, cluster = subclass)
cox_kidney_post <- coxph(Surv(KI_TIME_YRS, KI_EVENT) ~ HIV_POSITIVE,
                         data = post_df, robust = TRUE, cluster = subclass)

cox_liver_pre <- coxph(Surv(LI_TIME_YRS, LI_EVENT) ~ HIV_POSITIVE,
                       data = pre_df, robust = TRUE, cluster = subclass)
cox_liver_post <- coxph(Surv(LI_TIME_YRS, LI_EVENT) ~ HIV_POSITIVE,
                        data = post_df, robust = TRUE, cluster = subclass)

summary(cox_kidney_pre); summary(cox_kidney_post)
summary(cox_liver_pre);  summary(cox_liver_post)



Secondary Outcome Analysis: Hospital Length of Stay
```{r LOS Analysis}
#restrict LOS to patients discharged alive
los_df <- matched_df %>%
  filter(!is.na(los_days_inclusive))

# Fit negative binomial
nb_los <- glm.nb(
  los_days_inclusive ~ HIV_POSITIVE,
  data = los_df
)
nb_los
# Cluster-robust SEs by matched set
cov_clust <- vcovCL(nb_los, cluster = los_df$subclass)
nb_los_robust <- coeftest(nb_los, vcov = cov_clust)

nb_los_robust

#model adjusting for ERA
nb_los_era <- glm.nb(
  los_days_inclusive ~ HIV_POSITIVE + ERA,
  data = los_df
)

cov_clust_era <- vcovCL(nb_los_era, cluster = los_df$subclass)
coeftest(nb_los_era, vcov = cov_clust_era)

#sensitivity analysis: log-linear regression
lm_los <- lm(
  log(los_days_inclusive) ~ HIV_POSITIVE + ERA,
  data = los_df
)

coeftest(lm_los, vcov = vcovCL(lm_los, cluster = los_df$subclass))




hist(los_df$los_days_inclusive, breaks = 50)



# =========================
# 1) Fit the models (copy/paste and adapt if you already have them)
# =========================

# Make sure ERA exists and is coded correctly
matched_df$ERA <- factor(matched_df$ERA, levels = c("Pre-DAA", "Post-DAA"))

pre_df  <- subset(matched_df, ERA == "Pre-DAA")
post_df <- subset(matched_df, ERA == "Post-DAA")

# Adjusted pre/post models (these can sometimes warn if sparse)
res_pre  <- fit_cox_capture_warning(
  coxph(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~
          HIV_POSITIVE + REC_HBV_SURF_ANTIGEN_COMB + REC_CMV_COMBINED,
        data = pre_df,
        robust = TRUE,
        cluster = subclass)
)

res_post <- fit_cox_capture_warning(
  coxph(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~
          HIV_POSITIVE + REC_HBV_SURF_ANTIGEN_COMB + REC_CMV_COMBINED,
        data = post_df,
        robust = TRUE,
        cluster = subclass)
)

# Interaction model (pooled)
res_int <- fit_cox_capture_warning(
  coxph(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~
          HIV_POSITIVE * ERA + REC_HBV_SURF_ANTIGEN_COMB + REC_CMV_COMBINED,
        data = matched_df,
        robust = TRUE,
        cluster = subclass)
)

cox_pre      <- res_pre$fit
cox_post     <- res_post$fit
cox_int_adj  <- res_int$fit

