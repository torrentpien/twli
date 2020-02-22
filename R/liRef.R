liRef <- function(x, range, year, vbox = FALSE, done = TRUE) {

  range <- seq(range[1], range[2])

  record_year <- colnames(x)[grep("[0-9]{4}", colnames(x))[1]:ncol(x)][nchar(colnames(x)[grep("[0-9]{4}", colnames(x))[1]:ncol(x)]) == 4]

  range <- unique(c(range[1],  record_year[record_year %in% range], range[length(range)]))

  merge_temp <- dplyr::select(x, c(1:7), contains(range))

  merge_year <- intersect(record_year, range)

  merge_temp <- merge_temp[rowSums(is.na(merge_temp[, merge_year])) != length(merge_year),]

  #不同年度所需各種整併情況

  if (year == range[1]) { #檔案年度為資料處理範圍第一年

    if (year < merge_year[1]) {

      done = FALSE

    }

    if (done == TRUE) {

      chk_row <- data.frame(year = merge_year[1], mode = "vbox", mult = "0", stringsAsFactors = FALSE) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "s", mult = "1", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))
    }

    if (done == FALSE) {

      chk_row <- data.frame(year = merge_year[1], mode = "m", mult = "0", stringsAsFactors = FALSE) %>%
        bind_rows(data.frame(year = merge_year[1], mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[1], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "c", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "s", mult = "1", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))
    }

    if (length(range) >= 3) {

      mid_row <- data.frame(year = c(merge_year[2:(length(merge_year) - 1)]), mode = "m", mult = "0", stringsAsFactors = FALSE) %>%
        bind_rows(data.frame(year = c(merge_year[2:(length(merge_year) - 1)]), mode = "c", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = c(merge_year[2:(length(merge_year) - 1)]), mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = c(merge_year[2:(length(merge_year) - 1)]), mode = "s", mult = "1", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = c(merge_year[2:(length(merge_year) - 1)]), mode = "t", mult = "0", stringsAsFactors = FALSE))

      chk_row <- bind_rows(chk_row, mid_row)

    }

  }

  if (year == range[length(range)]) {

    if (year > merge_year[length(merge_year)]) {

      done = TRUE

    }

    if (done == TRUE) {

      chk_row <- data.frame(year = merge_year[1], mode = "vbox", mult = "0", stringsAsFactors = FALSE) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "m", mult = "1", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "s", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))
    }

    if (done == FALSE) {

      chk_row <- data.frame(year = merge_year[1], mode = "vbox", mult = "0", stringsAsFactors = FALSE) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))
    }

    if (length(merge_year) >= 3) {

      mid_row <- data.frame(year = c(merge_year[2:length[merge_year] - 1], mode = "s", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = c(merge_year[2:(length(merge_year) - 1)]), mode = "c", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = c(merge_year[2:(length(merge_year) - 1)]), mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = c(merge_year[2:(length(merge_year) - 1)]), mode = "t", mult = "0", stringsAsFactors = FALSE))

      chk_row <- bind_row(chk_row, mid_row)

    }
  }

  if (year > range[1] & year < range[length(range)]) { #資料年位於處理資料時間範圍的中間

    pre_year <- range[range < year] #資料年之前的年度，包括其他資料及x裡面的資料年

    if (length(setdiff(range, year)) > 0) { #檢測資料年之前的年度，裡面是否有比x裡面的資料年還前面的

      doc_year <- setdiff(pre_year, setdiff(range, year)) #x裡面的資料年之年度

      if (done == TRUE) {

        chk_row <- data.frame(year = doc_year, mode = "s", mult = "0", stringsAsFactors = FALSE) %>%
          bind_rows(data.frame(year = doc_year, mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = doc_year, mode = "c", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = doc_year, mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "m", mult = "1", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "s", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "c", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))

      }

      if (done == FALSE) {

        chk_row <- data.frame(year = doc_year, mode = "s", mult = "0", stringsAsFactors = FALSE) %>%
          bind_rows(data.frame(year = doc_year, mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = doc_year, mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "m", mult = "1", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "s", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))

      }

    }

    if (length(setdiff(range, year)) == 0) {

      doc_year <- pre_year

      if (done == TRUE) {

        chk_row <- data.frame(year = doc_year, mode = "vbox", mult = "0", stringsAsFactors = FALSE) %>%
          bind_rows(data.frame(year = doc_year, mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = doc_year, mode = "c", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "m", mult = "1", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "s", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "c", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "s", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))

      }

      if (done == FALSE) {

        chk_row <- data.frame(year = doc_year, mode = "vbox", mult = "0", stringsAsFactors = FALSE) %>%
          bind_rows(data.frame(year = doc_year, mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "c", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = year, mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
          bind_rows(data.frame(year = range[length(range)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))

      }

    }

  }

  if (vbox == FALSE) {

    chk_row <- chk_row %>%
      filter(mode != "vbox")

  }

  merge_list <- list()

  for (chk in 1:nrow(chk_row)) {

    #chk = 3
    mode_chk <- as.character(chk_row$mode[chk])
    #done_chk <- as.character(chk_row$done[chk])

    if (mode_chk == "vbox") {

      merge_chk <- merge_temp %>%
        filter(!!sym(paste0("mode_", chk_row$year[chk])) == mode_chk)

    } else {

      merge_chk <- merge_temp %>%
        filter(!!sym(paste0("mode_", chk_row$year[chk])) == mode_chk)


    }

    if (chk_row$mult[chk] == "1") {

      merge_chk <- merge_chk %>%
        filter(grepl("\\|", !!sym(chk_row$year[chk])) == TRUE)

    }

    if(nrow(merge_chk) > 0) {

      merge_chk$year <- chk_row$year[chk]

    }

    merge_list <- bind_rows(merge_list, merge_chk)

  }

  if (nrow(merge_list) == 0) {

    merge_gp <- list()

  }

  if (nrow(merge_list) > 0) {

    merge_gp <- list()
    ignore_row <- list()

    for (gp in 1:nrow(merge_list)) {

      #gp <- 2

      if (gp %in% ignore_row) {


      } else {

        if (length(grep("vbox", merge_list[gp,])) > 0) {

          idfy_col <- substr(colnames(merge_list[gp,])[grep("vbox", merge_list[gp,])[1]], regexpr("[0-9]+" ,colnames(merge_list[gp,])[grep("vbox", merge_list[gp,])[1]]),
                             nchar(colnames(merge_list[gp,])[grep("vbox", merge_list[gp,])[1]]))

          gp_list <- unlist(c(strsplit(unlist(merge_list[gp, idfy_col], use.names = FALSE), "|", fixed = TRUE), merge_list$li_id[gp]))

        } else {

          #從這裡開始，多里合併處理

          #gp_cand <- c(unlist(merge_list[gp, merge_list$year[gp]]), unlist(merge_list[gp, "li_id"]))

          gp_cand <- paste(c(unlist(merge_list[gp, merge_list$year[gp]]), unlist(merge_list[gp, "li_id"])), collapse = "|")

          gp_list <- unlist(strsplit(gp_cand, "|", fixed = TRUE))

          complete_row <- gp

          mult_lichk <- "1"

          while(length(mult_lichk) > 0) {

            idfy_row <- grep(gp_cand, unlist(merge_list[, merge_list$year[gp]]))

            complete_chk <- gp_list

            row_rd <- setdiff(idfy_row, complete_row)

            complete_row <- union(idfy_row, complete_row)

            #print("1")

            #gp_cand <- unique(unlist(c(merge_list[row_rd, merge_list$year[gp]], merge_list[row_rd, "li_id"])))

            gp_cand <- c(unlist(merge_list[row_rd, merge_list$year[gp]]), unlist(merge_list[row_rd, "li_id"]))

            mult_lichk <- unique(unlist(c(strsplit(unlist(gp_cand), "|", fixed = TRUE))))

            mult_lichk <- setdiff(mult_lichk, complete_chk)

            gp_list <- c(gp_list, mult_lichk)

            gp_cand <- paste(mult_lichk, collapse = "|")

            #print("2")

          }

          ignore_row <- union(ignore_row, complete_row)
          ignore_row <- setdiff(ignore_row, gp)

          gp_list <- c(sort(setdiff(gp_list, merge_list$li_id[complete_row])), merge_list$li_id[complete_row])

          #li_gp <- unlist(c(strsplit(unlist(merge_list[gp, merge_list$year[gp]]), "|", fixed = TRUE), merge_list$li_id[gp]))
          #li_gp <- strsplit(gp_list, "|", fixed = TRUE)

        }

        merge_gp_temp <- data.frame(li_id = gp_list, merge_code = seq(1:length(gp_list)), group = gp)

        merge_gp <- rbind(merge_gp, merge_gp_temp)

      }

    }

  }

  return(merge_gp)


}
