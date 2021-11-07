google <- function(x, level, url, dir, verbose){
  
  if(is.logical(url)){
    if(!url) return(x)
    url <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"
  }
  
  # backward compatibility
  if(is.null(x$key_google_mobility)){
    idx <- which(x$administrative_area_level==1)
    if(!length(idx)) return(x)
    x$key_google_mobility[idx] <- x$iso_alpha_2[idx]
  }
  
  if(length(idx <- which(x$administrative_area_level>1))){
    x$key_google_mobility[idx] <- paste(x$iso_alpha_2[idx], x$key_google_mobility[idx], sep = ", ")
    x$key_google_mobility[idx] <- gsub("(, (NA)?)+$", "", x$key_google_mobility[idx])    
  }

  # download
  path <- download(url, dir = dir, verbose = verbose, timestamp = TRUE)
  
  pattern <- sprintf("\\_%s\\_", paste0(unique(x$iso_alpha_2), collapse = "|"))
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  
  g <- data.table::rbindlist(fill = TRUE, lapply(files, function(file){
    g <- data.table::fread(file, encoding = "UTF-8", na.strings = "")
    g$key_google_mobility <- paste(g$country_region_code, g$sub_region_1, g$sub_region_2, g$metro_area, sep = ", ")
    g$key_google_mobility <- gsub("(, (NA)?)+$", "", g$key_google_mobility)
    g[g$key_google_mobility %in% x$key_google_mobility,]
  }))
  
  # date
  g$date <- as.Date(g$date)
  
  # return
  join(x, g, on = c("date" = "date", "key_google_mobility" = "key_google_mobility"))
  
}
