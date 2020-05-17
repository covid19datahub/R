apple <- function(x, level, url, cache){
  
  # check
  if(is.null(x$key_apple_mobility))
    return(x)
  
  # download
  a <- read.csv(url, cache = cache)
  
  # formatting
  by <- c("geo_type", "region", "transportation_type")
  cn <- colnames(a)
  by <- by[by %in% cn]
  cn <- (cn %in% by) | !is.na(as.Date(cn, format = "X%Y.%m.%d"))
  
  a <- a[,cn] %>%
    tidyr::pivot_longer(cols = -by, values_to = "value", names_to = "date") %>%
    tidyr::pivot_wider(names_from = "transportation_type", values_from = "value") %>%
    dplyr::mutate(date = as.Date(date, format = "X%Y.%m.%d"))
  
  # date
  a$date <- as.Date(a$date)
  
  # merge
  x <- merge(x, a, by.x = c("date","key_apple_mobility"), by.y = c("date","region"), all.x = TRUE)
    
  # drop 
  rm <- c("geo_type", "region")
  x  <- x[, setdiff(colnames(x), rm)]
  
  # return
  return(x)
  
}
