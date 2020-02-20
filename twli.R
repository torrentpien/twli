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
library(foreign)
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
setwd("~/Google Drive File Stream/我的雲端硬碟/stat/R/github/twli_code")

result_2009_ty <- read_xlsx("data/result_2009_ty.xlsx")
result_2010_kc <- read_xlsx("data/result_2010_kc.xlsx")
result_2014_kyc <- read_xlsx("data/result_2014_kyc.xlsx")
result_2018_kyc <- read_xlsx("data/result_2018_kyc.xlsx")


li_table <- read_xlsx("data/li_table.xlsx", sheet = "Sheet1", col_types = "text")

year_range <- c("2009", "2018")

file_year <- "2014"

merge_done <- c("TRUE")

vbox <- c("TRUE")

#村里合併表格函數

liRef <- function(x, range, year, vbox = FALSE, done = TRUE) {
  
  range <- seq(range[1], range[2])
  
  grep("[0-9]{4}", colnames(x))[1]
  
  year <- colnames(x)[grep("[0-9]{4}", colnames(x))[1]:ncol(x)][nchar(colnames(x)[grep("[0-9]{4}", colnames(x))[1]:ncol(x)]) == 4]
  
  range <- unique(c(range[1],  year[year %in% range], range[length(range)]))
  
  merge_temp <- dplyr::select(x, c(1:7), contains(range))
  
  merge_year <- intersect(year, range)
  
  merge_temp <- merge_temp[rowSums(is.na(merge_temp[, merge_year])) != length(merge_year),]
  
  #不同年度所需各種整併情況
  
  if (year == range[1]) { #檔案年度為資料處理範圍第一年
    
    if (year < merge_year[1]) {
      
      done = "FALSE"
      
    }
    
    if (done == "TRUE") { 
      
      chk_row <- data.frame(year = merge_year[1], mode = "vbox", mult = "0", stringsAsFactors = FALSE) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "s", mult = "1", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))
    } 
    
    if (done == "FALSE") {
      
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
      
      done = "done"
      
    }
    
    if (done == "TRUE") { 
      
      chk_row <- data.frame(year = merge_year[1], mode = "vbox", mult = "0", stringsAsFactors = FALSE) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "s", mult = "0", stringsAsFactors = FALSE)) %>%
        bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))
    } 
    
    if (done == "FALSE") {
      
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
      
      if (done == "TRUE") {
        
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
      
      if (done == "FALSE") {
        
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
      
      if (done == "TRUE") {
        
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
      
      if (done == "FALSE") {
        
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
  
  if (vbox == "FALSE") {
    
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
  
  return(merge_gp)
  
  
}

liRef(li_table, range = c("2009", "2018"), year = "2014")

# liRef 村里合併表格函數原始碼-----

range <- seq(range[1], range[2])

grep("[0-9]{4}", colnames(x))[1]

year <- colnames(x)[grep("[0-9]{4}", colnames(x))[1]:ncol(x)][nchar(colnames(x)[grep("[0-9]{4}", colnames(x))[1]:ncol(x)]) == 4]

range <- unique(c(range[1],  year[year %in% range], range[length(range)]))

merge_temp <- dplyr::select(x, c(1:7), contains(range))

merge_year <- intersect(year, range)

merge_temp <- merge_temp[rowSums(is.na(merge_temp[, merge_year])) != length(merge_year),]

#不同年度所需各種整併情況

if (year == range[1]) { #檔案年度為資料處理範圍第一年
  
  if (year < merge_year[1]) {
    
    done = "FALSE"
    
  }
  
  if (done == "TRUE") { 
    
    chk_row <- data.frame(year = merge_year[1], mode = "vbox", mult = "0", stringsAsFactors = FALSE) %>%
      bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "m", mult = "0", stringsAsFactors = FALSE)) %>%
      bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "s", mult = "1", stringsAsFactors = FALSE)) %>%
      bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
      bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))
  } 
  
  if (done == "FALSE") {
    
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
    
    done = "done"
    
  }
  
  if (done == "TRUE") { 
    
    chk_row <- data.frame(year = merge_year[1], mode = "vbox", mult = "0", stringsAsFactors = FALSE) %>%
      bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "t", mult = "0", stringsAsFactors = FALSE)) %>%
      bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "s", mult = "0", stringsAsFactors = FALSE)) %>%
      bind_rows(data.frame(year = merge_year[length(merge_year)], mode = "vbox", mult = "0", stringsAsFactors = FALSE))
  } 
  
  if (done == "FALSE") {
    
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
    
    if (done == "TRUE") {
      
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
    
    if (done == "FALSE") {
      
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
    
    if (done == "TRUE") {
      
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
    
    if (done == "FALSE") {
      
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

if (vbox == "FALSE") {
  
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

li_merge_table <- merge_gp

######村里整併加總函數----

liRef <- function(x, range, year, vbox = FALSE, done = TRUE)

liSum <- function(x, ref, cols) {
  
  mg_target <- x %>%
    left_join(ref, by = "li_id") %>%
    filter(is.na(group) == FALSE)
  
  mg_target <- mg_target %>%
    group_by(group) %>%
    summarise_at(vars(all_of(cols)), sum, na.rm = TRUE)
  
  mg_target <- mg_target %>%
    left_join(ref[ref$merge_code == "1",], by = "group")
  
  x[match(mg_target$li_id, x$li_id), cols] <- mg_target[, cols]
  
  x <- x %>%
    filter(!li_id %in% ref$li_id[ref$merge_code != 1])
  
  x <- x %>%
    mutate(li_adjust = case_when(li_id %in% ref$li_id[ref$merge_code == 1] ~ 1,
                                 TRUE ~ 0))
  return(x)
  
}

test <- merge_add(result_2014_kyc, c(2:9), li_merge_table)

# liSum 村里合併加總原始碼-----

col_range <- c(2:8)

col_name <- colnames(result_2009_ty)[col_range]

#merge_obj <- c("result_2009_ty")

mg_target <- result_2009_ty %>%
  left_join(merge_gp, by = "li_id") %>%
  filter(is.na(group) == FALSE)

mg_target <- mg_target %>%
  group_by(group) %>%
  summarise_at(vars(all_of(col_name)), sum, na.rm = TRUE)

mg_target <- mg_target %>%
  left_join(merge_gp[merge_gp$merge_code == "1",], by = "group")

result_2009_ty[match(mg_target$li_id, result_2009_ty$li_id), col_name] <- mg_target[, col_name]

result_2009_ty <- result_2009_ty %>%
  filter(!li_id %in% merge_gp$li_id[merge_gp$merge_code != 1])

result_2009_ty <- result_2009_ty %>%
  mutate(li_adjust = case_when(li_id %in% merge_gp$li_id[merge_gp$merge_code == 1] ~ 1,
                               TRUE ~ 0))

result_compare <- result_2009_ty %>%
  left_join(result_2018_kyc, by = "li_id")


#舊版

for (mg in 1:length(unique(merge_gp$group))) { 
  
  #mg = 1
  
  mg_temp <- merge_gp %>%
    filter(group == unique(merge_gp$group)[mg])
  
  mg_result <- result_2014_kyc %>%
    filter(li_id == mg_temp$li_id[1])
  
  mg_result[, col_range] <- colSums(result_2014_kyc[result_2014_kyc$li_id %in%mg_temp$li_id, col_range], na.rm = TRUE)
  
  result_2014_kyc[result_2014_kyc$li_id == mg_result$li_id,] <- mg_result
  
  result_2014_kyc$li_adjust <- "0"
  
  result_2014_kyc$li_adjust[result_2014_kyc$li_id == mg_result$li_id] <- "1"
  
  result_2014_kyc <- result_2014_kyc %>%
    filter(li_id != mg_temp$li_id[-1])
  
}


# 村里合併欄位運算函數------

liEqu <- function(x, ref, result, equ) {
  
  col_spl <- unlist(strsplit(gsub("\\s", "", equ), "/|-|\\(|\\)|\\*|\\^"))
  col_spl <- col_spl[nchar(col_spl) != 0]
  
  mg_target <- x %>%
    left_join(ref, by = "li_id") %>%
    filter(is.na(group) == FALSE)
  
  mg_target <- mg_target %>%
    group_by(group) %>%
    summarise_at(vars(all_of(col_spl)), sum, na.rm = TRUE)
  
  mg_target <- mg_target %>%
    mutate(!!result := !!parse_expr(equ))
  
  mg_target <- mg_target %>%
    left_join(ref[ref$merge_code == "1",], by = "group")
  
  x[match(mg_target$li_id, x$li_id), c(col_spl, result)] <- mg_target[, c(col_spl, result)]
  
  x <- x %>%
    filter(!li_id %in% ref$li_id[ref$merge_code != 1])
  
  x <- x %>%
    mutate(li_adjust = case_when(li_id %in% ref$li_id[ref$merge_code == 1] ~ 1,
                                 TRUE ~ 0))
  
  return(x)
  
}

# liEqu 村里合併欄位運算原始碼-----

#DPP_2014/(num_voter_2014 - invaild_vote_2014 - Other_2014)

fu <- "DPP_2014 / (num_voter_2014 - invaild_vote_2014 - Other_2014)"

equal <- c("DPP_rate_2014")

col_spl <- unlist(strsplit(gsub("\\s", "", fu), "/|-|\\(|\\)|\\*|\\^"))
col_spl <- col_spl[nchar(col_spl) != 0]

mg_target <- result_2014_kyc %>%
  left_join(merge_gp, by = "li_id") %>%
  filter(is.na(group) == FALSE)

mg_target <- mg_target %>%
  group_by(group) %>%
  summarise_at(vars(all_of(col_spl)), sum, na.rm = TRUE)

mg_target <- mg_target %>%
  mutate(!!equal := !!parse_expr(fu))

mg_target <- mg_target %>%
  left_join(merge_gp[merge_gp$merge_code == "1",], by = "group")

result_2014_kyc[match(mg_target$li_id, result_2014_kyc$li_id), c(col_spl, equal)] <- mg_target[, c(col_spl, equal)]

result_2014_kyc <- result_2014_kyc %>%
  filter(!li_id %in% merge_gp$li_id[merge_gp$merge_code != 1])

result_2014_kyc <- result_2014_kyc %>%
  mutate(li_adjust = case_when(li_id %in% merge_gp$li_id[merge_gp$merge_code == 1] ~ 1,
                               TRUE ~ 0))


## 村里合併shape檔處理函數-----

liShp <- function(x, ref) {
  
  for (g_idx in 1:length(unique(ref$group))) {
    
    agg_map <- aggregate(rbind(x[x$VILLAGE_ID %in% ref$li_id[ref$group == g_idx],]))
    
    maintain_li <- x@data[x$VILLAGE_ID == ref$li_id[ref$group == g_idx & ref$merge_code == "1"],]
    
    agg_map <- spChFIDs(agg_map, as.character(x$OBJECTID[x$VILLAGE_ID == ref$li_id[ref$group == g_idx & ref$merge_code == "1"]]))
    
    x <- x[!x$VILLAGE_ID %in% ref$li_id[ref$group == g_idx],]
    
    x <- spChFIDs(x, as.character(x$OBJECTID))
    
    x <- raster::bind(x, agg_map)
    
    x@data[nrow(x@data),] <- maintain_li
    
  }
  
  return(x)
  
}

#### liShp 村里合併shape檔處理-----



kh_map <- shapefile("data/Kaohsiung/VillageKH_NLSC_1041007.shp")

plot(kh_map)

li_adjusted <- data.frame(li_id = c("6403400-005", "6403400-006", "6403400-008", "6401200-045", "6401200-062"), merge_code = c("1", "2", "2", "1", "2"), group = c("1", "1", "1", "2", "2"))

map_adjusted <- kh_map

for (g_idx in 1:length(unique(li_adjusted$group))) {
  
  agg_map <- aggregate(rbind(map_adjusted[map_adjusted$VILLAGE_ID %in% li_adjusted$li_id[li_adjusted$group == g_idx],]))
  
  maintain_li <- map_adjusted@data[map_adjusted$VILLAGE_ID == li_adjusted$li_id[li_adjusted$group == g_idx & li_adjusted$merge_code == "1"],]
  
  agg_map <- spChFIDs(agg_map, as.character(map_adjusted$OBJECTID[map_adjusted$VILLAGE_ID == li_adjusted$li_id[li_adjusted$group == g_idx & li_adjusted$merge_code == "1"]]))
  
  map_adjusted <- map_adjusted[!map_adjusted$VILLAGE_ID %in% li_adjusted$li_id[li_adjusted$group == g_idx],]
  
  map_adjusted <- spChFIDs(map_adjusted, as.character(map_adjusted$OBJECTID))
  
  map_adjusted <- raster::bind(map_adjusted, agg_map)
  
  map_adjusted@data[nrow(map_adjusted@data),] <- maintain_li
  
}

shapefile(map_adjusted, "data/map_adjusted/Kaohsiung/kh_map_adjusted_20200217.shp", overwrite = TRUE)

#dbf <- read.dbf("data/map_adjusted/Kaohsiung/kh_map_adjusted_20191022.dbf")

#dbf <- map_adjusted@data

write.dbf(map_adjusted@data, "data/map_adjusted/Kaohsiung/kh_map_adjusted_20200217.dbf")

map_encoding <- shapefile("data/map_adjusted/Kaohsiung/kh_map_adjusted_20200217.shp")








####end


for (mg in 1:max(merge_gp$group)) {
  
  mg_temp <- merge_gp %>%
    filter(group == unique(merge_gp$group)[mg])
  
  mg_result <- get(merge_obj) %>%
    filter(li_id == mg_temp$li_id[1])
  
  #test <- get(merge_obj)[get(merge_obj)$li_id %in%mg_temp$li_id, ]
  
  mg_result[, col_range] <- colSums(get(merge_obj)[get(merge_obj)$li_id %in%mg_temp$li_id, col_range], na.rm = TRUE)
  
  eval(parse(text = paste0(get(merge_obj)[get(merge_obj)$li_id == mg_result$li_id,], "<-",  mg_result)))
  
  get(merge_obj)[get(merge_obj)$li_id == mg_result$li_id,] <- mg_result
  
  get(merge_obj)$li_adjust <- "0"
  
  get(merge_obj)$li_adjust[get(merge_obj)$li_id == mg_result$li_id] <- "1"
  
  get(merge_obj) <- get(merge_obj) %>%
    filter(li_id != mg_temp$li_id[-1])
  
}




write_xlsx(result_2009_ty, "result_2009_ty.xlsx")

write_xlsx(result_2010_kc, "result_2010_kc.xlsx")

result_2014_kyc <- result_fm_2014 %>%
  filter(substr(li_id, 1, 2) %in% c("64", "66", "68"))


write_xlsx(result_2014_kyc, "result_2014_kyc.xlsx")


write_xlsx(result_2018_kyc, "result_2018_kyc.xlsx")
