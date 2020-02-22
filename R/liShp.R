liShp <- function(x, ref) {

  for (g_idx in 1:length(unique(ref$group))) {

    agg_map <- stats::aggregate(rbind(x[x$li_id %in% ref$li_id[ref$group == g_idx],]))

    maintain_li <- x@data[x$li_id == ref$li_id[ref$group == g_idx & ref$merge_code == "1"],]

    agg_map <- sp::spChFIDs(agg_map, as.character(x$OBJECTID[x$li_id == ref$li_id[ref$group == g_idx & ref$merge_code == "1"]]))

    x <- x[!x$li_id %in% ref$li_id[ref$group == g_idx],]

    x <- sp::spChFIDs(x, as.character(x$OBJECTID))

    x <- raster::bind(x, agg_map)

    x@data[nrow(x@data),] <- maintain_li

  }

  return(x)

}
