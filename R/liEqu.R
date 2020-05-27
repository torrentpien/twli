liEqu <- function(x, ref, result, equ, finish = FALSE) {

  col_spl <- unlist(strsplit(gsub("\\s", "", equ), "/|-|\\(|\\)|\\*|\\^|\\+"))
  col_spl <- col_spl[nchar(col_spl) != 0]

  mg_target <- x %>%
    dplyr::left_join(ref, by = "li_id") %>%
    dplyr::filter(is.na(group) == FALSE)

  mg_target <- mg_target %>%
    dplyr::group_by(group) %>%
    dplyr::summarise_at(vars(all_of(col_spl)), sum, na.rm = TRUE)

  mg_target <- mg_target %>%
    dplyr::mutate(!!result := !!parse_expr(equ))

  mg_target <- mg_target %>%
    dplyr::left_join(ref[ref$merge_code == "1",], by = "group")

  x[match(mg_target$li_id, x$li_id), c(result)] <- mg_target[, c(result)]

  if (finish == TRUE) {

    x <- x %>%
      filter(!li_id %in% ref$li_id[ref$merge_code != 1])

    x <- x %>%
      dplyr::mutate(li_adjust = case_when(li_id %in% ref$li_id[ref$merge_code == 1] ~ 1,
                                   TRUE ~ 0))

  }

  return(x)

}
