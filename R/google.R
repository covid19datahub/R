google <- function(x, level, url, dir, verbose){
  
  # sanitize url
  if(is.logical(url)){
    if(!url) return(x)
    url <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"
  }
  
  # backward compatibility: use names instead of place_id
  backward <- any(nchar(x$key_google_mobility)!=27, na.rm = TRUE)
  if(backward){
  
    if(is.null(x$key_google_mobility)){
      idx <- which(x$administrative_area_level==1)
      if(!length(idx)) return(x)
      x$key_google_mobility[idx] <- x$iso_alpha_2[idx]
    }
    
    if(length(idx <- which(x$administrative_area_level>1))){
      x$key_google_mobility[idx] <- paste(x$iso_alpha_2[idx], x$key_google_mobility[idx], sep = ", ")
      x$key_google_mobility[idx] <- gsub("(, (NA)?)+$", "", x$key_google_mobility[idx])    
    }
    
  }

  # download
  path <- download(url, dir = dir, verbose = verbose, timestamp = TRUE)
  
  # files to read
  pattern <- sprintf("\\_%s\\_", paste0(unique(x$iso_alpha_2), collapse = "|"))
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  
  # read
  g <- data.table::rbindlist(fill = TRUE, lapply(files, function(file){
    g <- data.table::fread(file, encoding = "UTF-8", na.strings = "")
    if(backward){
      g$place_id <- paste(g$country_region_code, g$sub_region_1, g$sub_region_2, g$metro_area, sep = ", ")
      g$place_id <- gsub("(, (NA)?)+$", "", g$place_id)
    }
    g[g$place_id %in% x$key_google_mobility & !is.na(g$place_id),]
  }))
  
  # check
  if(!nrow(g))
    return(x)
  
  # convert date
  g$date <- as.Date(g$date)
  
  # subset
  g <- g[,c(
    "place_id", "date",            
    "retail_and_recreation_percent_change_from_baseline",
    "grocery_and_pharmacy_percent_change_from_baseline", 
    "parks_percent_change_from_baseline",    
    "transit_stations_percent_change_from_baseline",     
    "workplaces_percent_change_from_baseline",         
    "residential_percent_change_from_baseline")]
  
  # return
  join(x, g, on = c("date" = "date", "key_google_mobility" = "place_id"))
  
}
