#Now I combine the columns that are identical between liver and kidney text files, (come back to this and use de-duplicated list tx_slk_unique)
names(tx_slk_clean)

#identify name pairs
li_cols <- grep("_LI$", names(tx_slk_clean), value = TRUE)
ki_cols <- gsub("_LI$", "_KI", li_cols)
# keep only those where the KI version actually exists
paired <- li_cols[ki_cols %in% names(tx_slk_clean)]
pairs  <- tibble(li = paired, ki = gsub("_LI$", "_KI", paired))

#test the pairs for identical values
ident_results <- purrr::map2_df(
  pairs$li, pairs$ki,
  ~{
    v_li <- tx_slk_clean[[.x]]
    v_ki <- tx_slk_clean[[.y]]
    tibble(
      li_col    = .x,
      ki_col    = .y,
      pct_equal = mean(v_li == v_ki | (is.na(v_li) & is.na(v_ki)))
    )
  }
)
#keep only columns that match 100%
identical_pairs <- ident_results %>%
  filter(pct_equal == 1)

#create a copy of data set
tx_slk_combined <- tx_slk_clean

#combine identical pairs
for (i in seq_len(nrow(identical_pairs))) {
  li_name <- identical_pairs$li_col[i]
  ki_name <- identical_pairs$ki_col[i]
  base    <- sub("_LI$", "", li_name)
  
  tx_slk_combined[[base]] <- dplyr::coalesce(
    tx_slk_clean[[li_name]],
    tx_slk_clean[[ki_name]]
  )
  
  tx_slk_combined[[li_name]] <- NULL
  tx_slk_combined[[ki_name]] <- NULL
}





