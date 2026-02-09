

# Making condensed table for poster


matched_df <- matched_df %>%
  mutate(
    # Sex labels
    DON_GENDER = recode_sex_FM(DON_GENDER),
    CAN_GENDER = recode_sex_FM(CAN_GENDER),
    
    # Donor race collapsed to match recipient race buckets
    DON_RACE_collapsed = collapse_donor_race(DON_RACE),
  ) %>%
  mutate(
    # nice ordering for tables
    DON_GENDER = factor(DON_GENDER, levels = c("Female", "Male")),
    CAN_GENDER = factor(CAN_GENDER, levels = c("Female", "Male")),
    
    DON_RACE_collapsed = factor(DON_RACE_collapsed,
                                levels = c("White", "Black", "Hispanic/Latino", "Other", "Missing"))
  ) %>%
  set_variable_labels(
    DON_GENDER = "Donor sex",
    DON_RACE_collapsed = "Donor race",
    DON_ANTI_HIV_recode = "Donor anti-HIV antibody",
    
    CAN_GENDER = "Recipient sex",
    CAN_RACE_collapsed = "Recipient race",
    
    REC_HBV_ANTIBODY_COMB = "HBV core antibody",
    REC_HBV_SURF_ANTIGEN_COMB = "HBV surface antigen",
    REC_HCV_STAT_COMB = "HCV serology",
    REC_CMV_COMBINED = "CMV serology"
  )


matched_df <- matched_df %>%
  mutate(
    # ---- serologies: Unknown -> NA ----
    REC_HBV_ANTIBODY_COMB     = na_if(REC_HBV_ANTIBODY_COMB, "Unknown"),
    REC_HBV_SURF_ANTIGEN_COMB = na_if(REC_HBV_SURF_ANTIGEN_COMB, "Unknown"),
    REC_HCV_STAT_COMB         = na_if(REC_HCV_STAT_COMB, "Unknown"),
    REC_CMV_COMBINED          = na_if(REC_CMV_COMBINED, "Unknown"),
    
    # ---- donor anti-HIV: Unknown -> NA ----
    DON_ANTI_HIV_recode = na_if(DON_ANTI_HIV_recode, "Unknown"),
    
    # ---- drop unused factor levels ----
    across(where(is.factor), fct_drop)
  )


matched_df <- matched_df %>%
  mutate(
    DON_RACE_collapsed = case_when(
      DON_RACE %in% c("8", "White") ~ "White",
      DON_RACE %in% c("16", "Black or African American") ~ "Black",
      DON_RACE %in% c("2000", "Hispanic/Latino") ~ "Hispanic/Latino",
      is.na(DON_RACE) ~ NA_character_,
      TRUE ~ "Other"
    ),
    DON_RACE_collapsed = factor(
      DON_RACE_collapsed,
      levels = c("White", "Black", "Hispanic/Latino", "Other")
    )
  )


short_var_list<-c("DON_AGE",
                  "DON_GENDER",
                  "CAN_BMI_clean",
                  "REC_AGE_YEARS",
                  "CAN_GENDER",
                  "CAN_RACE_collapsed",
                  "TRANSPLANT_REASON_LI",
                  "TRANSPLANT_REASON_KI_COLLAPSED",
                  "REC_HBV_SURF_ANTIGEN_COMB",
                  "REC_HCV_STAT_COMB",
                  "REC_CMV_COMBINED",
                  "MELD_NUM_Last"
)



#condensed table
tbl_dat2 <- matched_df %>%
  dplyr::select(
    HIV_POSITIVE,
    all_of(short_var_list)
  )

# "self-healing" labels: only keep labels for vars that exist in tbl_dat
label_list_ok <- label_list[names(label_list) %in% names(tbl_dat)]


tab2 <-
  tbl_dat2 %>%
  gtsummary::tbl_summary(
    by = HIV_POSITIVE,
    missing = "ifany",
    statistic = list(
      gtsummary::all_continuous() ~ "{median} ({p25}, {p75})",
      gtsummary::all_categorical() ~ "{n} ({p}%)"
    ),
    label = label_list_ok
  ) %>%
  gtsummary::add_p(
    test = gtsummary::all_categorical() ~ "fisher.test",
    test.args = gtsummary::all_categorical() ~ list(simulate.p.value = TRUE)
  ) %>%
  as_gt() %>%
  gt::cols_label(
    stat_1 = "Negative/Unknown",
    stat_2 = "Positive"
  ) %>%
  gt::tab_header(
    title = "Characteristics of HIV-positive and HIV-negative SLK recipients (matched cohort)"
  )

gt::gtsave(tab2, "tables/matched_table_condensed_2.png")




#make tables
# Kidney graft failure events
with(matched_df, table(KI_EVENT, useNA = "ifany"))
# Liver graft failure events
with(matched_df, table(LI_EVENT, useNA = "ifany"))

# By era (optional)
with(matched_df, table(HIV_POSITIVE, KI_EVENT))
with(matched_df, table(HIV_POSITIVE, LI_EVENT))


#make kidney failure tables
#clean labels
t_kid_unadj <- clean_labels(mk_tbl(cox_kidney))
t_kid_era   <- clean_labels(mk_tbl(cox_kidney_era))
t_kid_int   <- clean_labels(mk_tbl(cox_kidney_int))
t_kid_pre   <- clean_labels(mk_tbl(cox_kidney_pre))
t_kid_post  <- clean_labels(mk_tbl(cox_kidney_post))

#create table
tbl_kidney_gf <-
  tbl_merge(
    tbls = list(
      t_kid_unadj,
      t_kid_era,
      t_kid_int,
      t_kid_pre,
      t_kid_post
    ),
    tab_spanner = c(
      "**Unadjusted**",
      "**Adjusted for ERA**",
      "**HIV × ERA**",
      "**Pre-DAA only**",
      "**Post-DAA only**"
    )
  ) %>%
  modify_caption("**Table 2. Cox proportional hazards models for kidney graft failure (matched cohort)**") %>%
  modify_footnote(
    everything() ~ "Robust standard errors clustered by matched set (subclass)."
  )

tbl_kidney_gf

#change table orientation
tbl_kidney_gf_long <- tbl_stack(
  tbls = list(t_kid_unadj, t_kid_era, t_kid_int, t_kid_pre, t_kid_post),
  group_header = c(
    "Unadjusted",
    "Adjusted for ERA",
    "HIV × ERA interaction",
    "Pre-DAA only",
    "Post-DAA only"
  )
) %>%
  modify_caption("**Table 2. Cox proportional hazards models for kidney graft failure (matched cohort)**") %>%
  modify_footnote(everything() ~ "Robust standard errors clustered by matched set (subclass).")

tbl_kidney_gf_long


#make liver tables
t_liv_unadj <- clean_labels(mk_tbl(cox_liver))
t_liv_era   <- clean_labels(mk_tbl(cox_liver_era))
t_liv_pre   <- clean_labels(mk_tbl(cox_liver_pre))
t_liv_post  <- clean_labels(mk_tbl(cox_liver_post))

tbl_liver_gf <-
  tbl_merge(
    tbls = list(
      t_liv_unadj,
      t_liv_era,
      t_liv_pre,
      t_liv_post
    ),
    tab_spanner = c(
      "**Unadjusted**",
      "**Adjusted for ERA**",
      "**Pre-DAA only**",
      "**Post-DAA only**"
    )
  ) %>%
  modify_caption("**Table 3. Cox proportional hazards models for liver graft failure (matched cohort)**") %>%
  modify_footnote(
    everything() ~
      "Robust standard errors clustered by matched set (subclass). Post-DAA estimates may be unstable due to sparse events."
  )

tbl_liver_gf

#change table orientation
tbl_liver_gf_long <- tbl_stack(
  tbls = list(t_liv_unadj, t_liv_era, t_liv_pre, t_liv_post),
  group_header = c(
    "Unadjusted",
    "Adjusted for ERA",
    "Pre-DAA only",
    "Post-DAA only"
  )
) %>%
  modify_caption("**Table 3. Cox proportional hazards models for liver graft failure (matched cohort)**") %>%
  modify_footnote(
    everything() ~ "Robust standard errors clustered by matched set (subclass). Post-DAA estimates may be unstable due to sparse events."
  )

tbl_liver_gf_long

```

table for poster
```{r making tables for poster}
library(gtsummary)
library(survival)
library(gt)

# -----------------------------
# 0) Helper functions
# -----------------------------
mk_tbl <- function(fit) {
  tbl_regression(
    fit,
    exponentiate = TRUE,
    estimate_fun = ~ style_sigfig(.x, digits = 2),   # keeps model output tidy
    pvalue_fun   = ~ style_pvalue(.x, digits = 2)
  ) %>%
    bold_labels()
}

clean_labels <- function(tbl) {
  tbl %>%
    modify_table_body(~ .x %>%
                        mutate(
                          label = case_when(
                            label == "HIV_POSITIVE" ~ "HIV-positive (vs HIV-negative)",
                            TRUE ~ label
                          )
                        )
    )
}

# Create HR (95% CI) with 2 decimals and keep p-value
shrink_one <- function(tbl) {
  tbl %>%
    modify_table_body(~ .x %>%
                        # keep only the HIV row
                        filter(term == "HIV_POSITIVE") %>%
                        mutate(
                          est_num  = suppressWarnings(as.numeric(estimate)),
                          low_num  = suppressWarnings(as.numeric(conf.low)),
                          high_num = suppressWarnings(as.numeric(conf.high)),
                          
                          hr_ci = case_when(
                            !is.na(est_num) ~ sprintf("%.2f (%.2f–%.2f)", est_num, low_num, high_num),
                            TRUE ~ NA_character_
                          ),
                          
                          p_fmt = case_when(
                            is.na(p.value) ~ NA_character_,
                            p.value < 0.01 ~ "<0.01",
                            TRUE ~ sprintf("%.2f", p.value)
                          )
                        ) %>%
                        select(label, hr_ci, p_fmt)
    ) %>%
    modify_header(
      hr_ci ~ "**HR (95% CI)**",
      p_fmt ~ "**p**"
    )
}


# Mark Post-DAA liver graft failure as not estimable (NE)
mark_ne <- function(tbl) {
  tbl %>%
    modify_table_body(~ .x %>%
                        mutate(
                          hr_ci = "NE",
                          p_fmt = NA_character_
                        )
    )
}

# -----------------------------
# 1) Build SMALL tables for each model (overall / pre / post) per outcome
# -----------------------------

# ---- Mortality ----
mort_overall <- clean_labels(mk_tbl(cox_main))       %>% shrink_one()
mort_pre     <- clean_labels(mk_tbl(cox_pre_unadj))  %>% shrink_one()
mort_post    <- clean_labels(mk_tbl(cox_post_unadj)) %>% shrink_one()

tbl_mort_block <- tbl_merge(
  tbls = list(mort_overall, mort_pre, mort_post),
  tab_spanner = c("**Overall**", "**Pre-DAA**", "**Post-DAA**")
)

# ---- Kidney graft failure ----
kid_overall <- clean_labels(mk_tbl(cox_kidney))      %>% shrink_one()
kid_pre     <- clean_labels(mk_tbl(cox_kidney_pre))  %>% shrink_one()
kid_post    <- clean_labels(mk_tbl(cox_kidney_post)) %>% shrink_one()

tbl_kid_block <- tbl_merge(
  tbls = list(kid_overall, kid_pre, kid_post),
  tab_spanner = c("**Overall**", "**Pre-DAA**", "**Post-DAA**")
)

# ---- Liver graft failure ----
liv_overall <- clean_labels(mk_tbl(cox_liver))      %>% shrink_one()
liv_pre     <- clean_labels(mk_tbl(cox_liver_pre))  %>% shrink_one()

# Post-DAA liver: quasi separation -> NE
liv_post <- clean_labels(mk_tbl(cox_liver_post)) %>%
  shrink_one() %>%
  mark_ne()

tbl_liv_block <- tbl_merge(
  tbls = list(liv_overall, liv_pre, liv_post),
  tab_spanner = c("**Overall**", "**Pre-DAA**", "**Post-DAA**")
)

# -----------------------------
# 2) Stack into the MEGA table + caption/footnote
# -----------------------------
tbl_mega <- tbl_stack(
  tbls = list(tbl_mort_block, tbl_kid_block, tbl_liv_block),
  group_header = c(
    "Overall mortality",
    "Kidney graft failure (28 events; 6 HIV+)",
    "Liver graft failure (16 events; 2 HIV+)"
  )
) %>%
  modify_caption("**Cox regression results (matched cohort), overall and stratified by DAA era**") %>%
  modify_footnote(
    everything() ~ paste(
      "Robust SEs clustered by matched set (subclass).",
      "Kidney graft failure: 28 total events (6 HIV+).",
      "Liver graft failure: 16 total events (2 HIV+).",
      "Post-DAA liver graft failure: NE = not estimable due to sparse/zero due to sparse/zero events (quasi separation)."
    )
  )

tbl_mega

# -----------------------------
# 3) Save as PNG for poster
# -----------------------------
gt_tbl <- tbl_mega %>%
  as_gt() %>%
  tab_options(
    table.width = pct(100),
    table.font.size = 11,
    data_row.padding = px(3)
  )

gtsave(gt_tbl, filename = "tables/cox_mega_table_poster.png")



interaction_df <- bind_rows(
  extract_interaction(cox_mort_int,   "Overall mortality"),
  extract_interaction(cox_kidney_int, "Kidney graft failure (28 events; 6 HIV+)"),
  extract_interaction(cox_liver_int,  "Liver graft failure (16 events; 2 HIV+)")
)


mega_df_final <- mega_df %>%
  left_join(interaction_df, by = "Outcome")

gt_tbl <- mega_df_final %>%
  gt(rowname_col = "Outcome") %>%
  tab_header(
    title = md("**Cox regression results (matched cohort; N=336 total, 56 HIV-positive)**"),
    subtitle = md("Overall, stratified by DAA era, and HIV × era interaction")
  ) %>%
  tab_source_note(
    md("Post-DAA liver graft failure: NE = not estimable due to sparse/zero due to sparse/zero events (quasi separation). Interaction ratio is the ratio of post-DAA to pre-DAA hazard ratios")
  ) %>%
  tab_options(
    table.width = pct(80),
    table.font.size = 11,
    data_row.padding = px(3)
  )

gtsave(gt_tbl, "cox_mega_table_with_interaction.png")







# ---- build LONG table ----
los_long_tbl <- dplyr::bind_rows(
  extract_robust(
    nb_los,
    cluster = los_df$subclass,
    model_label = "Negative binomial (unadjusted)",
    exponentiate = TRUE,
    keep_terms = c("HIV_POSITIVE")
  ),
  extract_robust(
    nb_los_era,
    cluster = los_df$subclass,
    model_label = "Negative binomial (+ ERA)",
    exponentiate = TRUE,
    keep_terms = c("HIV_POSITIVE", "ERAPost-DAA")
  ),
  extract_robust(
    lm_los,
    cluster = los_df$subclass,
    model_label = "Log-linear sensitivity (log LOS, + ERA)",
    exponentiate = TRUE,
    keep_terms = c("HIV_POSITIVE", "ERAPost-DAA")
  )
)

# ---- render as grouped long table ----
los_gt <- los_long_tbl %>%
  gt(groupname_col = "Model") %>%
  tab_header(title = "Length of hospital stay (discharged alive): regression results") %>%
  cols_label(
    Predictor = "Predictor",
    `Effect (95% CI)` = "Ratio (95% CI)",
    `Approx. % diff`  = "Approx. % difference",
    `p-value` = "p-value"
  ) %>%
  tab_source_note("Effect estimates are ratios (exp(beta)); robust SEs clustered by matched set (subclass). Outcome: los_days_inclusive.")

los_gt




# =========================
# 2) Create a single clean table (HR (95% CI) + p)
# =========================
tab_pre  <- format_cox(cox_pre,  "Pre-DAA (adjusted)")
tab_post <- format_cox(cox_post, "Post-DAA (adjusted)")
tab_int  <- format_cox(cox_int_adj, "All eras (HIV×ERA + adjusted)")

all_tabs <- bind_rows(tab_pre, tab_post, tab_int)

print(all_tabs)

# =========================
# 3) Export to a text file (easy to email)
# =========================
out_dir <- "~/Desktop/slk_match"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

txt_file <- file.path(out_dir, "SLK_Cox_PrePost_Interaction_Summary.txt")

sink(txt_file)
cat("SLK Cox Models (Matched Cohort)\n")
cat("=========================================\n\n")

cat("NOTE: Models use robust SEs clustered by matched set (subclass).\n")
cat("      Matching weights are all 1 (nearest neighbor without replacement).\n\n")

if (length(res_pre$warnings)) {
  cat("Warnings (Pre-DAA model):\n")
  cat(paste0("- ", unique(res_pre$warnings), collapse = "\n"), "\n\n")
}
if (length(res_post$warnings)) {
  cat("Warnings (Post-DAA model):\n")
  cat(paste0("- ", unique(res_post$warnings), collapse = "\n"), "\n\n")
}
if (length(res_int$warnings)) {
  cat("Warnings (Interaction model):\n")
  cat(paste0("- ", unique(res_int$warnings), collapse = "\n"), "\n\n")
}

cat("Table: Hazard ratios (95% CI) and p-values\n\n")
print(all_tabs, row.names = FALSE)

cat("\n\n--- Full model output (verbatim) ---\n\n")
cat("\n[Pre-DAA adjusted model]\n")
print(summary(cox_pre))
cat("\n[Post-DAA adjusted model]\n")
print(summary(cox_post))
cat("\n[All eras interaction + adjusted model]\n")
print(summary(cox_int_adj))

sink()

message("Saved: ", txt_file)

# =========================
# 4) (Optional) Also export as CSV for quick viewing
# =========================
csv_file <- file.path(out_dir, "SLK_Cox_PrePost_Interaction_Table.csv")
write.csv(all_tabs, csv_file, row.names = FALSE)
message("Saved: ", csv_file)


