library(dplyr)
library(tidyr)
library(stringr)
library(rlang)
library(spdep)
library(rgdal)
library(plm)
library(lmtest)
library(GISTools)
library(raster)
library(maptools)
library(readr)
library(readxl)
library(writexl)
library(xlsx)
library(stargazer)
library(ggplot2)
library(jsonlite)
library(leaflet)
library(htmlwidgets)

options(warn = -1)

#setwd('D:/Users/Torrent/Google 雲端硬碟/stat/R/github/twli')
setwd('/media/torrent/Database/Users/torrent/Google 雲端硬碟/stat/R/github/twli')
setwd("/media/torrent/Database/User/Google 雲端硬碟/stat/R/github/twli")
setwd("~/Google Drive File Stream/我的雲端硬碟/stat/R/github/twli")

result_2009_ty <- read_xlsx("data/result_2009_ty.xlsx")
result_2010_kc <- read_xlsx("data/result_2010_kc.xlsx")
result_2014_kyc <- read_xlsx("data/result_2014_kyc.xlsx")
result_2018_kyc <- read_xlsx("data/result_2018_kyc.xlsx")


li_table <- read_xlsx("data/li_table.xlsx", sheet = "Sheet1", col_types = "text")

year_range <- c("2009", "2018")

file_year <- "2014"

merge_done <- c("TRUE")

#村里合併表格函數

merge_ref <- function(li_table, year_range, file_year) {
  
  year_range <- seq(year_range[1], year_range[2])
  
  year <- colnames(li_table)[8:ncol(li_table)][nchar(colnames(li_table)[8:ncol(li_table)]) == 4]
  
  year_range <- year[year %in% year_range]
  
  merge_temp <- dplyr::select(li_table, c(1:7), contains(year_range))
  
  merge_temp <- merge_temp[rowSums(is.na(merge_temp[,year_range])) != length(year_range),]
  
  if (file_year == year_range[1]) {
    
    chk_row <- matrix(c(
      year_range[1], "m", NA,
      year_range[1], "vbox", NA,
      year_range[length(year_range)], "m", NA, 
      year_range[length(year_range)], "vbox", NA), ncol = 3, byrow = TRUE)
    
    chk_row <- as.data.frame(chk_row)
    
    colnames(chk_row) <- c("year", "mode", "done")
    
    if (length(year_range) >= 3) {
      
      mid_row <- data.frame(year = c(year_range[2:length[year_range] - 1], mode = c("m", "vbox"), done = NA))
      chk_row <- bind_row(chk_row, mid_row)
      
    }
  }
  
  if (file_year == year_range[length(year_range)]) {
    
    chk_row <- matrix(c(
      year_range[length(year_range)], "s", "v",
      year_range[1], "vbox", NA,
      year_range[length(year_range)], "vbox", NA), ncol = 3, byrow = TRUE)
    
    chk_row <- as.data.frame(chk_row)
    
    colnames(chk_row) <- c("year", "mode", "done")
    
    if (length(year_range) >= 3) {
      
      mid_row <- data.frame(year = c(year_range[2:length[year_range] - 1], mode = c("s", "vbox"), done = NA))
      chk_row <- bind_row(chk_row, mid_row)
      
    }
  }
  
  
  merge_list <- list()
  
  for (chk in 1:nrow(chk_row)) {
    
    mode_chk <- as.character(chk_row$mode[chk])
    done_chk <- as.character(chk_row$done[chk])
    
    if (mode_chk == "vbox") {
      
      merge_chk <- merge_temp %>%
        filter(!!sym(paste0("mode_", chk_row$year[chk])) == mode_chk)
      
    } else {
      
      merge_chk <- merge_temp %>%
        filter(!!sym(paste0("mode_", chk_row$year[chk])) == mode_chk)
      
      if(is.na(done_chk) == TRUE) {
        
        merge_chk <- merge_chk[is.na(merge_chk[, paste0("done_", chk_row$year[chk])]) == TRUE,]
        
      } else {
        
        merge_chk <- merge_chk[merge_chk[, paste0("done_", chk_row$year[chk])] == done_chk,]
        
      }
      
      
    }
    
    merge_list <- bind_rows(merge_list, merge_chk)
    
  }
  
  merge_gp <- list()
  
  for (gp in 1:nrow(merge_list)) {
    
    if (length(grep("vbox", merge_list[gp,])) > 0) {
      
      idfy_col <- substr(colnames(merge_list[gp,])[grep("vbox", merge_list[gp,])[1]], regexpr("[0-9]+" ,colnames(merge_list[gp,])[grep("vbox", merge_list[gp,])[1]]),
                         nchar(colnames(merge_list[gp,])[grep("vbox", merge_list[gp,])[1]]))
      
      li_gp <- unlist(c(strsplit(unlist(merge_list[gp, idfy_col]), "|", fixed = TRUE), merge_list$li_id[gp]))
      
      
    } else {
      
      li_gp <- unlist(c(strsplit(unlist(merge_list[gp, file_year]), "|", fixed = TRUE), merge_list$li_id[gp]))
      
    }
    
    merge_gp_temp <- data.frame(li_id = li_gp, merge_code = seq(1:length(li_gp)), group = gp)
    
    merge_gp <- rbind(merge_gp, merge_gp_temp)
    
  }
  
  return(merge_gp)
  
  
}

li_merge_table <- merge_ref(li_table, c(2014, 2018), "2018")

#村里合併表格函數原始碼-----

year_range <- seq(year_range[1], year_range[2])

year <- colnames(li_table)[8:ncol(li_table)][nchar(colnames(li_table)[8:ncol(li_table)]) == 4]

year_range <- unique(c(year_range[1],  year[year %in% year_range], year_range[length(year_range)]))

merge_temp <- dplyr::select(li_table, c(1:7), contains(year_range))

merge_temp <- merge_temp[rowSums(is.na(merge_temp[, year])) != length(year),]

#不同年度所需各種整併情況

if (file_year == year_range[1]) { #檔案年度為資料處理範圍第一年
  
  if (merge_done == "TRUE") { 
    
    chk_row <- data.frame(year = year_range[1], mode = "vbox", stringsAsFactors = FALSE) %>%
      bind_rows(data.frame(year = year_range[length(year_range)], mode = "m", stringsAsFactors = FALSE)) %>%
      bind_rows(data.frame(year = year_range[length(year_range)], mode = "vbox", stringsAsFactors = FALSE))
  } 
  
  if (merge_done == "FALSE") {
    
    chk_row <- data.frame(year = year_range[1], mode = "m", stringsAsFactors = FALSE) %>%
      bind_rows(data.frame(year = year_range[1], mode = "vbox", stringsAsFactors = FALSE)) %>%
      bind_rows(data.frame(year = year_range[length(year_range)], mode = "m", stringsAsFactors = FALSE)) %>%
      bind_rows(data.frame(year = year_range[length(year_range)], mode = "vbox", stringsAsFactors = FALSE))
  } 
  
  if (length(year_range) >= 3) {
    
    mid_row <- data.frame(year = c(year_range[2:length[year_range] - 1], mode = c("m", "vbox")))
    chk_row <- bind_row(chk_row, mid_row)
    
  }
  
}

if (file_year == year_range[length(year_range)]) {
  
  if (merge_done == "TRUE") { 
    
    chk_row <- data.frame(year = year_range[1], mode = "vbox", stringsAsFactors = FALSE) %>%
      bind_rows(data.frame(year = year_range[length(year_range)], mode = "s", stringsAsFactors = FALSE)) %>%
      bind_rows(data.frame(year = year_range[length(year_range)], mode = "vbox", stringsAsFactors = FALSE))
  } 
  
  if (merge_done == "FALSE") {
    
    chk_row <- data.frame(year = year_range[1], mode = "vbox", stringsAsFactors = FALSE) %>%
      bind_rows(data.frame(year = year_range[length(year_range)], mode = "m", stringsAsFactors = FALSE)) %>%
      bind_rows(data.frame(year = year_range[length(year_range)], mode = "vbox", stringsAsFactors = FALSE))
  } 
  
  if (length(year_range) >= 3) {
    
    mid_row <- data.frame(year = c(year_range[2:length[year_range] - 1], mode = c("s", "vbox"), done = NA))
    chk_row <- bind_row(chk_row, mid_row)
    
  }
}

if (file_year > year_range[1] & file_year < year_range[length(year_range)]) {
  
  pre_year <- year_range[year_range < file_year]
  
  if (length(setdiff(year_range, year)) > 0) {
    
    doc_year <- setdiff(pre_year, setdiff(year_range, year))
    
    if (merge_done == "TRUE") {
      
      chk_row <- data.frame(year = doc_year, mode = "s", mult = "0", stringsAsFactors = FALSE) %>%
        bind_rows(data.frame(year = doc_year, mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = doc_year, mode = "c", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = file_year, mode = "m", mult = "1", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = file_year, mode = "s", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = file_year, mode = "c", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = file_year, mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = year_range[length(year_range)], mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = year_range[length(year_range)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))
      
    }
    
    if (merge_done == "FALSE") {
      
      chk_row <- data.frame(year = doc_year, mode = "s", mult = "0", stringsAsFactors = FALSE) %>%
        bind_rows(data.frame(year = doc_year, mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = file_year, mode = "m", mult = "1", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = file_year, mode = "s", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = file_year, mode = "vbox", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = year_range[length(year_range)], mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = year_range[length(year_range)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))
      
    }
    
}
  
  
 
  
}

merge_list <- list()

for (chk in 1:nrow(chk_row)) {
  
  #chk = 6
  mode_chk <- as.character(chk_row$mode[chk])
  #done_chk <- as.character(chk_row$done[chk])
  
  if (mode_chk == "vbox") {
    
    merge_chk <- merge_temp %>%
      filter(!!sym(paste0("mode_", chk_row$year[chk])) == mode_chk)
    
  } else {
    
    merge_chk <- merge_temp %>%
      filter(!!sym(paste0("mode_", chk_row$year[chk])) == mode_chk)
    
      #merge_chk <- merge_chk %>%
      #  filter(grepl("\\|", !!sym(chk_row$year[chk])) == TRUE)
    
  }
  
  if(nrow(merge_chk) > 0) {
    
    merge_chk$year <- chk_row$year[chk]
  
  }
  
  merge_list <- bind_rows(merge_list, merge_chk)
  
}

merge_gp <- list()
ignore_row <- list()

for (gp in 1:nrow(merge_list)) {
  
  gp <- 1 
  
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
        
        print("1")
        
        #gp_cand <- unique(unlist(c(merge_list[row_rd, merge_list$year[gp]], merge_list[row_rd, "li_id"])))
        
        gp_cand <- c(unlist(merge_list[row_rd, merge_list$year[gp]]), unlist(merge_list[row_rd, "li_id"]))
        
        mult_lichk <- unique(unlist(c(strsplit(unlist(gp_cand), "|", fixed = TRUE))))
        
        mult_lichk <- setdiff(mult_lichk, complete_chk)
        
        gp_list <- c(gp_list, mult_lichk)
        
        gp_cand <- paste(mult_lichk, collapse = "|")
        
        print("2")
        
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



#村里整併加總函數

col_range <- c(5:12)

merge_obj <- c("result_2018_kyc")

merge_add <- function(data, col_range, merge_gp) {
  
  for (mg in 1:max(merge_gp$group)) { 
    
    #mg = 1
  
    mg_temp <- merge_gp %>%
    filter(group == unique(merge_gp$group)[mg])
    
    mg_result <- get(merge_obj) %>%
    filter(li_id == mg_temp$li_id[1])
    
    mg_result[, col_range] <- colSums(data[data$li_id %in%mg_temp$li_id, col_range], na.rm = TRUE)
    
    data[data$li_id == mg_result$li_id,] <- mg_result
    
    data$li_adjust <- "0"
    
    data$li_adjust[data$li_id == mg_result$li_id] <- "1"
    
    data <- data %>%
      filter(li_id != mg_temp$li_id[-1])
    
  
  }
  
  return(data)
  
}

test <- merge_add(result_2018_kyc, c(5:12), li_merge_table)


for (mg in 1:max(merge_gp$group)) {
  
  mg_temp <- merge_gp %>%
    filter(group == unique(merge_gp$group)[mg])
  
  mg_result <- get(merge_obj) %>%
    filter(li_id == mg_temp$li_id[1])
  
  test <- get(merge_obj)[get(merge_obj)$li_id %in%mg_temp$li_id, ]
  
  mg_result[, col_range] <- colSums(get(merge_obj)[get(merge_obj)$li_id %in%mg_temp$li_id, col_range], na.rm = TRUE)
  
  eval(parse(text = paste0(get(merge_obj)[get(merge_obj)$li_id == mg_result$li_id,], "<-",  mg_result)))
  
  get(merge_obj)[get(merge_obj)$li_id == mg_result$li_id,] <- mg_result
  
  get(merge_obj)$li_adjust <- "0"
  
  get(merge_obj)$li_adjust[get(merge_obj)$li_id == mg_result$li_id] <- "1"
  
  get(merge_obj) <- get(merge_obj) %>%
    filter(li_id != mg_temp$li_id[-1])
  
}

####end



write_xlsx(result_2009_ty, "result_2009_ty.xlsx")

write_xlsx(result_2010_kc, "result_2010_kc.xlsx")

result_2014_kyc <- result_fm_2014 %>%
  filter(substr(li_id, 1, 2) %in% c("64", "66", "68"))


write_xlsx(result_2014_kyc, "result_2014_kyc.xlsx")


write_xlsx(result_2018_kyc, "result_2018_kyc.xlsx")
