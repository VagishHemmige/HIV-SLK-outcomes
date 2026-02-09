#Functions


#From the combining_liver_kidney_variables.R script
combine_li_ki_consistent <- function(li, ki,
                                     pos_codes = c("Y","P","1"),
                                     neg_codes = c("N","0"),
                                     unknown_codes = c("U","ND","Unknown","", NA),
                                     out_pos = NULL,
                                     out_neg = NULL,
                                     unknown_out = NA_character_,
                                     discordant_out = NA_character_) {
  
  li_chr <- stringr::str_trim(as.character(li))
  ki_chr <- stringr::str_trim(as.character(ki))
  
  # Clean unknowns
  li_clean <- ifelse(li_chr %in% unknown_codes, NA_character_, li_chr)
  ki_clean <- ifelse(ki_chr %in% unknown_codes, NA_character_, ki_chr)
  
  if (is.null(out_pos)) out_pos <- pos_codes[1]
  if (is.null(out_neg)) out_neg <- neg_codes[1]
  
  discordant <- (!is.na(li_clean) & !is.na(ki_clean)) &
    ((li_clean %in% pos_codes & ki_clean %in% neg_codes) |
       (li_clean %in% neg_codes & ki_clean %in% pos_codes))
  
  dplyr::case_when(
    # Explicit P vs N conflict
    discordant ~ discordant_out,
    
    # Positive if P + (P or NA)
    (li_clean %in% pos_codes & (is.na(ki_clean) | ki_clean %in% pos_codes)) ~ out_pos,
    (ki_clean %in% pos_codes & (is.na(li_clean) | li_clean %in% pos_codes)) ~ out_pos,
    
    # Negative if N + N OR N + NA
    (li_clean %in% neg_codes & (is.na(ki_clean) | ki_clean %in% neg_codes)) ~ out_neg,
    (ki_clean %in% neg_codes & (is.na(li_clean) | li_clean %in% neg_codes)) ~ out_neg,
    
    # Everything else
    TRUE ~ unknown_out
  )
}



# --- helper recoders ---
recode_sex_FM <- function(x) {
  x <- as.character(x)
  dplyr::recode(x,
                "F" = "Female",
                "M" = "Male",
                .default = x
  )
}

recode_YNU_to_words <- function(x) {
  x <- as.character(x)
  # combine U + Unknown first
  x <- dplyr::recode(x, "Unknown" = "U", .default = x)
  dplyr::recode(x,
                "Y" = "Yes",
                "N" = "No",
                "U" = "Unknown",
                .default = x
  )
}


# SRTR-style donor race codes seen in your table:
collapse_donor_race <- function(x) {
  x <- as.character(x)
  case_when(
    is.na(x) ~ "Missing",
    x %in% c("8", "White", "WHITE") ~ "White",
    x %in% c("16", "Black or African American", "BLACK") ~ "Black",
    x %in% c("2000", "Hispanic/Latino", "HISPANIC") ~ "Hispanic/Latino",
    # everything else (Asian, AIAN, NHPI, etc.)
    TRUE ~ "Other"
  )
}


#Converts a vector of labels into a list object for the gtsummary package
make_gtsummary_labels <- function(named_vec) {
  lapply(names(named_vec), function(v) {
    as.formula(paste0(v, " ~ ", shQuote(unname(named_vec[[v]]))))
  })
}


# Helper: convert NA / "" / " " to "Missing" and return a factor
make_missing_level <- function(x, missing_label = "Missing") {
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA
  x[is.na(x)] <- missing_label
  factor(x)
}

#fix variable levels
recode_YNU <- function(x) {
  recode(x,
         "Y" = "Yes",
         "N" = "No",
         "U" = "Unknown",
         .default = as.character(x))
}

recode_PNU <- function(x) {
  recode(x,
         "P" = "Positive",
         "N" = "Negative",
         "U" = "Unknown",
         .default = as.character(x))
}


#Table-creating functions

#make a "make table" function
mk_tbl <- function(fit) {
  tbl_regression(
    fit,
    exponentiate = TRUE
  ) %>%
    bold_labels() %>%
    italicize_levels()
}

#make clean labels function
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





# ---***---***---***---***---***---***---***---***---***---***---***---***---***---***
# Helper: extract robust HR/CI/p for HIV_POSITIVE
# Uses vcov(fit) -> robust sandwich when you fit with robust=TRUE + cluster=subclass
# ---***---***---***---***---***---***---***---***---***---***---***---***---***---***

extract_hiv_row <- function(fit, label, ne = FALSE) {
  if (ne) {
    return(tibble(
      Outcome = label,
      `HR (95% CI)` = "NE",
      p = ""
    ))
  }
  
  term <- "HIV_POSITIVE"
  b <- unname(coef(fit)[term])
  
  # robust SE from vcov (should be robust due to robust=TRUE + cluster=subclass)
  V <- vcov(fit)
  se <- sqrt(unname(V[term, term]))
  
  z  <- b / se
  p  <- 2 * pnorm(-abs(z))
  
  hr <- exp(b)
  lo <- exp(b - 1.96 * se)
  hi <- exp(b + 1.96 * se)
  
  p_fmt <- ifelse(is.na(p), "",
                  ifelse(p < 0.01, "<0.01", sprintf("%.2f", p)))
  
  tibble(
    Outcome = label,
    `HR (95% CI)` = sprintf("%.2f (%.2f–%.2f)", hr, lo, hi),
    p = p_fmt
  )
}


# ---***---***---***---***---***---***---***---***---***---***---***---***---***---***
# Build the mega results table as a dataframe
# Assumes you already created these Cox fits:
# cox_main, cox_pre_unadj, cox_post_unadj
# cox_kidney, cox_kidney_pre, cox_kidney_post
# cox_liver, cox_liver_pre, cox_liver_post
# ---***---***---***---***---***---***---***---***---***---***---***---***---***---***

mega_df <- bind_rows(
  # Mortality block (overall/pre/post)
  extract_hiv_row(cox_main,      "Overall mortality — Overall") %>% mutate(ERA = "Overall"),
  extract_hiv_row(cox_pre_unadj, "Overall mortality — Pre-DAA") %>% mutate(ERA = "Pre-DAA"),
  extract_hiv_row(cox_post_unadj,"Overall mortality — Post-DAA")%>% mutate(ERA = "Post-DAA"),
  
  # Kidney graft failure block with event counts in label
  extract_hiv_row(cox_kidney,      "Kidney graft failure (28 events; 6 HIV+) — Overall") %>% mutate(ERA = "Overall"),
  extract_hiv_row(cox_kidney_pre,  "Kidney graft failure (28 events; 6 HIV+) — Pre-DAA") %>% mutate(ERA = "Pre-DAA"),
  extract_hiv_row(cox_kidney_post, "Kidney graft failure (28 events; 6 HIV+) — Post-DAA")%>% mutate(ERA = "Post-DAA"),
  
  # Liver graft failure block with Post-DAA marked NE
  extract_hiv_row(cox_liver,     "Liver graft failure (16 events; 2 HIV+) — Overall") %>% mutate(ERA = "Overall"),
  extract_hiv_row(cox_liver_pre, "Liver graft failure (16 events; 2 HIV+) — Pre-DAA") %>% mutate(ERA = "Pre-DAA"),
  extract_hiv_row(cox_liver_post,"Liver graft failure (16 events; 2 HIV+) — Post-DAA", ne = TRUE) %>% mutate(ERA = "Post-DAA")
) %>%
  # Keep just the outcome group name (without the era suffix) and make a wide table
  mutate(
    OutcomeGroup = sub(" — (Overall|Pre-DAA|Post-DAA)$", "", Outcome)
  ) %>%
  select(OutcomeGroup, ERA, `HR (95% CI)`, p) %>%
  tidyr::pivot_wider(
    names_from = ERA,
    values_from = c(`HR (95% CI)`, p),
    names_glue = "{.value} ({ERA})"
  ) %>%
  rename(Outcome = OutcomeGroup)





# ---***---***---***---***---***---***---***---***---***---***---***---***---***---***
# Render with gt and save
# ---***---***---***---***---***---***---***---***---***---***---***---***---***---***

gt_tbl <- mega_df %>%
  gt(rowname_col = "Outcome") %>%
  tab_header(
    title = md("**Cox regression results (matched cohort), overall and stratified by DAA era**"),
    subtitle = md("HIV-positive vs HIV-negative; robust SEs clustered by matched set (subclass)")
  ) %>%
  tab_source_note(
    md("Kidney graft failure: 28 total events (6 HIV+). Liver graft failure: 16 total events (2 HIV+). Post-DAA liver graft failure: **NE** = not estimable due to sparse/zero events (quasi separation).")
  ) %>%
  tab_options(
    table.width = pct(80),
    table.font.size = 11,
    data_row.padding = px(3)
  )

gt_tbl

gtsave(gt_tbl, filename = "cox_mega_table_poster.png")

#adding interaction term to table
cox_mort_int <- coxph(
  Surv(REC_DEATH_yrs, REC_DEATH_BINARY) ~ HIV_POSITIVE * ERA,
  data = matched_df,
  robust = TRUE,
  cluster = subclass
)

cox_kidney_int <- coxph(
  Surv(KI_TIME_YRS, KI_EVENT) ~ HIV_POSITIVE * ERA,
  data = matched_df,
  robust = TRUE,
  cluster = subclass
)

cox_liver_int <- coxph(
  Surv(LI_TIME_YRS, LI_EVENT) ~ HIV_POSITIVE * ERA,
  data = matched_df,
  robust = TRUE,
  cluster = subclass
)

library(dplyr)
library(survival)

fmt_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < 0.01) return("<0.01")
  sprintf("%.2f", p)
}

fmt_hrci <- function(hr, lo, hi) {
  sprintf("%.2f (%.2f–%.2f)", hr, lo, hi)
}



extract_interaction <- function(fit, outcome_label,
                                term = "HIV_POSITIVE:ERAPost-DAA") {
  
  # ---- FORCE NE for liver graft failure (sparse post-DAA events) ----
  if (outcome_label == "Liver graft failure (16 events; 2 HIV+)") {
    return(tibble(
      Outcome = outcome_label,
      `Interaction HR (95% CI)` = "NE",
      `Interaction p` = NA_character_
    ))
  }
  
  # ---- If interaction term not present ----
  if (!term %in% names(coef(fit))) {
    return(tibble(
      Outcome = outcome_label,
      `Interaction HR (95% CI)` = NA_character_,
      `Interaction p` = NA_character_
    ))
  }
  
  
  
  b <- unname(coef(fit)[term])
  
  V <- tryCatch(vcov(fit), error = function(e) NULL)
  if (is.null(V) || !all(term %in% rownames(V))) {
    return(tibble(
      Outcome = outcome_label,
      `Interaction HR (95% CI)` = NA_character_,
      `Interaction p` = NA_character_
    ))
  }
  
  se <- sqrt(unname(V[term, term]))
  
  # ---- Guard against infinite / zero / NA SE ----
  if (!is.finite(se) || is.na(se) || se <= 0) {
    return(tibble(
      Outcome = outcome_label,
      `Interaction HR (95% CI)` = NA_character_,
      `Interaction p` = NA_character_
    ))
  }
  
  # ---- Compute HR, CI, p ----
  hr <- exp(b)
  lo <- exp(b - 1.96 * se)
  hi <- exp(b + 1.96 * se)
  p  <- 2 * pnorm(-abs(b / se))
  
  tibble(
    Outcome = outcome_label,
    `Interaction HR (95% CI)` = sprintf("%.2f (%.2f–%.2f)", hr, lo, hi),
    `Interaction p` = ifelse(p < 0.01, "<0.01", sprintf("%.2f", p))
  )
}

#make table for LOS
extract_robust <- function(fit, cluster, model_label,
                           exponentiate = TRUE,
                           keep_terms = NULL) {
  
  ct <- coeftest(fit, vcov = vcovCL(fit, cluster = cluster))
  
  df <- tibble(
    Model    = model_label,
    term     = rownames(ct),
    estimate = as.numeric(ct[, 1]),
    se       = as.numeric(ct[, 2]),
    p.value  = as.numeric(ct[, ncol(ct)])
  )
  
  if (!is.null(keep_terms)) df <- df %>% dplyr::filter(term %in% keep_terms)
  
  df <- df %>%
    dplyr::mutate(
      conf.low  = estimate - 1.96 * se,
      conf.high = estimate + 1.96 * se
    )
  
  if (exponentiate) {
    df <- df %>%
      dplyr::mutate(
        est = exp(estimate),
        lo  = exp(conf.low),
        hi  = exp(conf.high),
        pct = (est - 1) * 100
      )
  } else {
    df <- df %>%
      dplyr::mutate(
        est = estimate,
        lo  = conf.low,
        hi  = conf.high,
        pct = NA_real_
      )
  }
  
  df %>%
    dplyr::mutate(
      Predictor = dplyr::case_when(
        term == "HIV_POSITIVE" ~ "HIV-positive (vs HIV-negative)",
        term == "ERAPost-DAA"  ~ "Post-DAA era (vs Pre-DAA)",
        term == "(Intercept)" ~ "Intercept",
        TRUE ~ term
      ),
      `Effect (95% CI)` = sprintf("%.2f (%.2f, %.2f)", est, lo, hi),
      `Approx. % diff`  = ifelse(is.na(pct), NA_character_, sprintf("%+.1f%%", pct)),
      `p-value` = dplyr::case_when(
        is.na(p.value) ~ NA_character_,
        p.value < 0.001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", p.value)
      )
    ) %>%
    dplyr::select(Model, Predictor, `Effect (95% CI)`, `Approx. % diff`, `p-value`)
}


# --- helper: format one cox model nicely ---

format_cox <- function(fit, model_name = "Model") {
  s <- summary(fit)
  n <- s$n
  events <- s$nevent
  
  # broom table (robust SEs are already baked into coef(summary(fit)) when robust=TRUE)
  out <- broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(
      model = model_name,
      n = n,
      events = events,
      # nicer labels
      term = case_when(
        term == "HIV_POSITIVE" ~ "HIV positive (vs negative)",
        term == "ERAPost-DAA" ~ "Post-DAA (vs Pre-DAA)",
        term == "HIV_POSITIVE:ERAPost-DAA" ~ "Interaction: HIV × Post-DAA",
        str_detect(term, "^REC_HBV_SURF_ANTIGEN_COMB") ~ str_replace(term, "^REC_HBV_SURF_ANTIGEN_COMB", "HBsAg: "),
        str_detect(term, "^REC_CMV_COMBINED") ~ str_replace(term, "^REC_CMV_COMBINED", "CMV: "),
        TRUE ~ term
      ),
      hr_ci = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high),
      p_fmt = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    ) %>%
    select(model, n, events, term, hr_ci, p_fmt)
  
  out
}

# --- helper: capture warnings while fitting (optional) ---
fit_cox_capture_warning <- function(expr) {
  w <- NULL
  fit <- withCallingHandlers(
    expr,
    warning = function(m) {
      w <<- c(w, conditionMessage(m))
      invokeRestart("muffleWarning")
    }
  )
  list(fit = fit, warnings = w)
}

