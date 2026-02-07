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
