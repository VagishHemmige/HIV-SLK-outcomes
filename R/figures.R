#Figures

# Make KM curves -- overall, pre and post DAA

# Ensure HIV is a factor with nice labels

matched_df <- matched_df %>%
  mutate(
    HIV_LABEL = factor(
      HIV_POSITIVE,
      levels = c(0, 1),
      labels = c("HIV Negative", "HIV Positive")
    )
  )

# Make ERA a factor with consistent order
matched_df$ERA <- factor(matched_df$ERA, levels = c("Pre-DAA", "Post-DAA"))

#Overall KM curve with risk table 
km_all <- survfit(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = matched_df)

p_all <- ggsurvplot(
  km_all,
  data = matched_df,
  risk.table = TRUE,
  risk.table.height = 0.28,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Years post-transplant",
  ylab = "Survival probability",
  title = "Overall survival after SLK (HIV+ vs HIV-)",
  legend.title = "",
  legend.labs = c("HIV Negative", "HIV Positive"),
  break.time.by = 2
)

print(p_all)

# Save (curve + risk table together)
g_all <- arrange_ggsurvplots(list(p_all), print = FALSE)

ggsave(
  filename = "KM_overall.png",
  plot     = g_all,
  width    = 15,
  height   = 7,
  dpi      = 300
)

#Pre DAA KM curve with risk table
pre_df <- matched_df %>% filter(ERA == "Pre-DAA")

km_pre <- survfit(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = pre_df)

p_pre <- ggsurvplot(
  km_pre,
  data = pre_df,
  risk.table = TRUE,
  risk.table.height = 0.28,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Years post-transplant",
  ylab = "Survival probability",
  title = "Survival after SLK (Pre-DAA era, 2005-2013)",
  legend.title = "",
  legend.labs = c("HIV Negative", "HIV Positive"),
  break.time.by = 2
)

print(p_pre)

# Save (curve + risk table together)
g_all_pre <- arrange_ggsurvplots(list(p_pre), print = FALSE)

ggsave(
  filename = "KM_pre_DAA.png",
  plot     = g_all_pre,
  width    = 15,
  height   = 7,
  dpi      = 300
)


#Post DAA KM Plot
post_df <- matched_df %>% filter(ERA == "Post-DAA")

km_post <- survfit(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = post_df)

p_post <- ggsurvplot(
  km_post,
  data = post_df,
  risk.table = TRUE,
  risk.table.height = 0.28,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Years post-transplant",
  ylab = "Survival probability",
  title = "Survival after SLK (Post-DAA era, 2014-2023)",
  legend.title = "",
  legend.labs = c("HIV Negative", "HIV Positive"),
  break.time.by = 2
)

print(p_post)

# Save (curve + risk table together)
g_all_post <- arrange_ggsurvplots(list(p_post), print = FALSE)

ggsave(
  filename = "KM_post_DAA.png",
  plot     = g_all_post,
  width    = 15,
  height   = 7,
  dpi      = 300
)

#Log-Rank tests
lr_all  <- survdiff(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = matched_df)
lr_pre  <- survdiff(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = pre_df)
lr_post <- survdiff(Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE, data = post_df)

lr_all
lr_pre
lr_post

#median survival tables 
summary(km_all)$table
summary(km_pre)$table
summary(km_post)$table

#median survival tables

# Overall
km_all_tbl <- as.data.frame(summary(km_all)$table) %>%
  tibble::rownames_to_column("Group") %>%
  dplyr::mutate(
    Era = "Overall",
    HIV = if_else(grepl("=1", Group), "HIV-positive", "HIV-negative")
  )

# Pre-DAA
km_pre_tbl <- as.data.frame(summary(km_pre)$table) %>%
  tibble::rownames_to_column("Group") %>%
  dplyr::mutate(
    Era = "Pre-DAA",
    HIV = if_else(grepl("=1", Group), "HIV-positive", "HIV-negative")
  )

# Post-DAA
km_post_tbl <- as.data.frame(summary(km_post)$table) %>%
  tibble::rownames_to_column("Group") %>%
  dplyr::mutate(
    Era = "Post-DAA",
    HIV = if_else(grepl("=1", Group), "HIV-positive", "HIV-negative")
  )

# Combine + clean
km_median_tbl <- dplyr::bind_rows(
  km_all_tbl,
  km_pre_tbl,
  km_post_tbl
) %>%
  dplyr::mutate(
    median_fmt = ifelse(
      is.na(median),
      "Not reached",
      sprintf("%.2f", median)
    ),
    CI = ifelse(
      is.na(median),
      "—",
      paste0(sprintf("%.2f", `0.95LCL`), ", ", sprintf("%.2f", `0.95UCL`))
    )
  ) %>%
  dplyr::select(
    Era,
    HIV,
    N = records,
    Events = events,
    `Median survival (years)` = median_fmt,
    `95% CI` = CI
  )

km_median_tbl


km_all_tbl <- as.data.frame(summary(km_all)$table) %>%
  tibble::rownames_to_column("Group") %>%
  dplyr::mutate(
    Era = "Overall",
    HIV = if_else(grepl("=1", Group), "HIV-positive", "HIV-negative")
  )

km_pre_tbl <- as.data.frame(summary(km_pre)$table) %>%
  tibble::rownames_to_column("Group") %>%
  dplyr::mutate(
    Era = "Pre-DAA",
    HIV = if_else(grepl("=1", Group), "HIV-positive", "HIV-negative")
  )

km_post_tbl <- as.data.frame(summary(km_post)$table) %>%
  tibble::rownames_to_column("Group") %>%
  dplyr::mutate(
    Era = "Post-DAA",
    HIV = if_else(grepl("=1", Group), "HIV-positive", "HIV-negative")
  )

km_median_tbl <- dplyr::bind_rows(km_all_tbl, km_pre_tbl, km_post_tbl) %>%
  dplyr::mutate(
    median_fmt = ifelse(is.na(median), "Not reached", sprintf("%.2f", median)),
    CI = ifelse(
      is.na(median),
      "—",
      paste0(sprintf("%.2f", `0.95LCL`), ", ", sprintf("%.2f", `0.95UCL`))
    )
  ) %>%
  dplyr::select(
    Era,
    HIV,
    N = records,
    Events = events,
    `Median survival (years)` = median_fmt,
    `95% CI` = CI
  )

# Print as a clean, copyable text table
print(knitr::kable(km_median_tbl, format = "simple"))



