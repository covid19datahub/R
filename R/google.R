google <- function(x, level, url, cache){
  
  # check
  if(is.null(x$key_google_mobility)){
    
    if(level==1)
      x$key_google_mobility <- x$iso_alpha_2
    else
      return(x)
    
  }
    
  # clean
  x$key_google_mobility <- gsub(", NA$", "", x$key_google_mobility)
  
  # download
  g <- read.csv(url, cache = cache)
  
  # date
  g$date <- as.Date(g$date)
  
  # level
  if(level==1){
    
    g <- g[!is.na(g$country_region) & is.na(g$sub_region_1) & is.na(g$sub_region_2),]
    
    x <- merge(x, g, by.x = c("date","key_google_mobility"), by.y = c("date","country_region_code"), all.x = TRUE)
    
  }
  if(level==2){
    
    g <- g[!is.na(g$country_region) & !is.na(g$sub_region_1) & is.na(g$sub_region_2),]
    
    g$key_google_mobility <- paste(g$country_region_code, g$sub_region_1, sep = ", ")
    x$key_google_mobility <- paste(x$iso_alpha_2, x$key_google_mobility, sep = ", ")
    
    x <- merge(x, g, by = c("date","key_google_mobility"), all.x = TRUE)
    
  }
  if(level==3){
    
    g <- g[!is.na(g$country_region) & !is.na(g$sub_region_1) & !is.na(g$sub_region_2),]
    
    g$key_google_mobility <- paste(g$country_region_code, g$sub_region_1, g$sub_region_2, sep = ", ")
    x$key_google_mobility <- paste(x$iso_alpha_2, x$key_google_mobility, sep = ", ")
    
    x <- merge(x, g, by = c("date","key_google_mobility"), all.x = TRUE)
    
  }
  
  # drop 
  rm <- c("country_region_code","country_region","sub_region_1","sub_region_2")
  x  <- x[, setdiff(colnames(x), rm)]

  # return
  return(x)
  
}