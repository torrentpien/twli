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

setwd('D:/Users/Torrent/Google 雲端硬碟/stat/R/github/twli_code')
setwd('/media/torrent/Database/Users/torrent/Google 雲端硬碟/stat/R/github/twli_code')
setwd("/media/torrent/Database/User/Google 雲端硬碟/stat/R/github/twli")
setwd("~/Google Drive File Stream/我的雲端硬碟/stat/R/github/twli_code")

result_2009_ty <- read_xlsx("data/result_2009_ty.xlsx")
result_2010_kc <- read_xlsx("data/result_2010_kc.xlsx")
result_2014_kyc <- read_xlsx("data/result_2014_kyc.xlsx")
result_2018_kyc <- read_xlsx("data/result_2018_kyc.xlsx")


li_table <- read_xlsx("data/li_table.xlsx", sheet = "Sheet1", col_types = "text")
x <- read_xlsx("data/li_fongshan.xlsx", col_types = "text")

range <- c("2010", "2014")

year <- "2014"

done <- c("TRUE")

vbox <- c("TRUE")

# liRef 村里合併表格函數原始碼-----

range <- seq(range[1], range[2])

record_year <- colnames(x)[grep("[0-9]{4}", colnames(x))[1]:ncol(x)][nchar(colnames(x)[grep("[0-9]{4}", colnames(x))[1]:ncol(x)]) == 4]

range <- unique(c(range[1],  record_year[record_year %in% range], range[length(range)]))

merge_temp <- dplyr::select(x, c(1:7), dplyr::matches(paste(range, collapse = "|")))

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





li_merge_table <- merge_gp


# liSum 村里合併加總原始碼-----

col_range <- c(2:8)

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



# liEqu 村里合併欄位運算原始碼-----

#DPP_2014/(num_voter_2014 - invaild_vote_2014 - Other_2014)

fu <- "DPP_2014 / (num_voter_2014 - invaild_vote_2014 - Other_2014)"
fu <- "(KMT_2010 + Other_2010) / vaild_vote_2010"

equal <- c("KMT_rate_2010")

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

x[match(mg_target$li_id, x$li_id), c(col_spl, result)] <- mg_target[, c(col_spl, result)]

if (finish == TRUE) {
  
  x <- x %>%
    filter(!li_id %in% ref$li_id[ref$merge_code != 1])
  
  x <- x %>%
    dplyr::mutate(li_adjust = case_when(li_id %in% ref$li_id[ref$merge_code == 1] ~ 1,
                                        TRUE ~ 0))
  
}


#### liShp 村里合併shape檔處理-----


for (g_idx in 1:length(unique(ref$group))) {
  
  agg_map <- stats::aggregate(rbind(x[x$li_id %in% ref$li_id[ref$group == g_idx],]))
  
  maintain_li <- x@data[x$li_id == ref$li_id[ref$group == g_idx & ref$merge_code == "1"],]
  
  agg_map <- sp::spChFIDs(agg_map, as.character(x$OBJECTID[x$li_id == ref$li_id[ref$group == g_idx & ref$merge_code == "1"]]))
  
  x <- x[!x$li_id %in% ref$li_id[ref$group == g_idx],]
  
  x <- sp::spChFIDs(x, as.character(x$OBJECTID))
  
  x <- raster::bind(x, agg_map)
  
  x@data[nrow(x@data),] <- maintain_li
  
}


#舊版

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

#案例-----

fongshan_2010 <- read.csv("data/fongshan_2010.csv", stringsAsFactors = FALSE)
fongshan_2014 <- read.csv("data/fongshan_2014.csv", stringsAsFactors = FALSE)

li_fongshan <- read_xlsx("data/li_fongshan.xlsx", col_types = c("text"))

fs_ref_2010 <- liRef(li_fongshan, range = c("2010", "2014"), year = "2010")

fs_ref_2014 <- liRef(li_fongshan, range = c("2010", "2014"), year = "2014")

fs_2014_sum <- liSum(fongshan_2014, ref = fs_ref_2014, cols = c(2:9), finish = TRUE)

fs_2010_sum <- liSum(fongshan_2010, ref = fs_ref_2010, cols = c(5:11), finish = TRUE)


fs_2010_rate <- liEqu(fongshan_2010, ref = fs_ref, result = "DDP_rate_2010", equ = c("DPP_2010 / vaild_vote_2010"))

fs_2010_rate <- liEqu(fs_2010_rate, ref = fs_ref, result = "KMT_rate_2010", equ = c("(KMT_2010 + Other_2010) / vaild_vote_2010"), finish = TRUE)

compare_test <- fs_2010_sum %>%
  full_join(fs_2014_sum, by = "li_id")
3
fs_map <- shapefile("data/map_adjusted/fongshan/fs_map.shp")

plot(fs_map)

kh_map <- shapefile("data/Kaohsiung/VillageKH_NLSC_1041007.shp")

fs_map <- kh_map[kh_map$TOWN_ID == "6401200",]



fs_map <- shapefile("data/map_adjusted/fongshan/fs_map.shp")

colnames(fs_map@data)[6] <- "li_id"

fs_map_df <- fortify(fs_map, region = "li_id")

fs_map_df$merge[fs_map_df$id == c("6401200-045")] <- "1"
fs_map_df$merge[fs_map_df$id == c("6401200-062")] <- "2"

map <- ggplot(data = fs_map_df, aes(x = long, y = lat, group = group))

map + geom_path(color = "white") +
  ggtitle("高雄市鳳山區圖資整併前") +
  geom_polygon(aes(fill = merge)) +
  coord_fixed(1.3) +
  coord_equal() +
  scale_fill_discrete(name = NULL,
                      labels = c("生明里","誠智里","其它")) +
  theme(text = element_text(family = "Heiti TC Medium"),
        plot.background=element_rect(fill="white", colour=NA),
        aspect.ratio = 1) +
  xlab(NULL) + ylab(NULL)

ggsave(filename = "fs.png", width = 4, 
       dpi = 200, plot = last_plot(), device='png'
)

fs_map_adjust <- liShp(fs_map, fs_ref_2014)

fs_map_ad_df <- fortify(fs_map_adjust, region = "li_id")

#fs_map_df <- merge(fs_map_df, fs_ref_2014, by.x = "id", by.y = "li_id", all.x = TRUE)

fs_map_ad_df$merge[fs_map_ad_df$id == c("6401200-045")] <- "1"
fs_map_ad_df$merge[fs_map_ad_df$id == c("6401200-062")] <- "2"

map_ad <- ggplot(data = fs_map_ad_df, aes(x = long, y = lat, group = group))

map_ad + geom_path(color = "white") +
  ggtitle("高雄市鳳山區圖資整併後") +
  geom_polygon(aes(fill = merge)) +
  coord_fixed(1.3) +
  coord_equal() +
  scale_fill_discrete(name = NULL,
                      labels = c("生明里","其它")) +
  theme(text = element_text(family = "Heiti TC Medium"),
        plot.background=element_rect(fill="white", colour=NA),
        aspect.ratio = 1) +
  xlab(NULL) + ylab(NULL)

ggsave(filename = "fs_a.png", width = 4, 
       dpi = 200, plot = last_plot(), device='png'
)

  with(centroids, annotate(geom="text", x = clong, y = clat, label = centroids$name, size = 2))
  
  

+
  geom_text(data=centroids, aes(fontface=2 ,centroids$clong, centroids$clat , label = name), 
            color= "black" ,size=3, check_overlap = T, position=position_jitter(width=3, height=3)) 
  
  
  
  guides(fill = guide_legend(order = 1))
  
  
  
+



shapefile(fs_map, "data/map_adjusted/fongshan/fs_map.shp", overwrite = TRUE)

write.dbf(fs_map@data, "data/map_adjusted/fongshan/fs_map.dbf")

map_encoding <- shapefile("data/map_adjusted/fongshan/fs_map.shp")

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
