apple <- function(x, level, url, cache){
  
  # check
  if(is.null(x$key_apple_mobility))
    return(x)
  
  # download
  a <- read.csv(url, cache = cache)
  
  # formatting
  by <- c("geo_type", "region", "sub.region", "transportation_type")
  cn <- colnames(a)
  by <- by[by %in% cn]
  cn <- (cn %in% by) | !is.na(as.Date(cn, format = "X%Y.%m.%d"))
  
  a <- a[,cn] %>%
    tidyr::pivot_longer(cols = -by, values_to = "value", names_to = "date") %>%
    tidyr::pivot_wider(names_from = "transportation_type", values_from = "value") %>%
    dplyr::mutate(date = as.Date(date, format = "X%Y.%m.%d"))
  
  # date
  a$date <- as.Date(a$date)
  
  # key
  a$key_apple_mobility <- gsub(", NA$", "", paste(a$region, a$sub.region, sep = ", "))
  
  # merge
  x <- merge(x, a, by = c("date","key_apple_mobility"), all.x = TRUE)
    
  # drop 
  rm <- c("geo_type", "region", "sub.region")
  x  <- x[, setdiff(colnames(x), rm)]
  
  # return
  return(x)
  
}
