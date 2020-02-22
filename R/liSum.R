liSum <- function(x, ref, cols, finish = FALSE) {

  col_name <- colnames(x)[cols]

  mg_target <- x %>%
    left_join(ref, by = "li_id") %>%
    filter(is.na(group) == FALSE)

  mg_target <- mg_target %>%
    group_by(group) %>%
    summarise_at(vars(all_of(col_name)), sum, na.rm = TRUE)

  mg_target <- mg_target %>%
    left_join(ref[ref$merge_code == "1",], by = "group")

  x[match(mg_target$li_id, x$li_id), col_name] <- mg_target[, col_name]

  if (finish == TRUE) {

    x <- x %>%
      filter(!li_id %in% ref$li_id[ref$merge_code != 1])

    x <- x %>%
      mutate(li_adjust = case_when(li_id %in% ref$li_id[ref$merge_code == 1] ~ 1,
                                   TRUE ~ 0))

  }

  return(x)

}
