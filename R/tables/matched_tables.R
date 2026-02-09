#Matched tables

label_list <- make_gtsummary_labels(var_labels)


tbl_dat <- matched_df %>%
  dplyr::select(
    HIV_POSITIVE,
    all_of(Don_Var_List),
    all_of(Rec_Li_Var_List),
    all_of(Rec_Ki_Var_List),
    all_of(Cand_Li_Var_List),
    all_of(Cand_Ki_Var_List)
  )

# "self-healing" labels: only keep labels for vars that exist in tbl_dat
label_list_ok <- label_list[names(label_list) %in% names(tbl_dat)]


tab1 <-
  tbl_dat %>%
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

gt::gtsave(tab1, "tables/matched_table_df_full.png")
