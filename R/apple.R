apple <- function(x, level, url, dir, verbose){
  
  # check
  if(is.null(x$key_apple_mobility))
    return(x)
  
  # read
  a <- data.table::fread(url, encoding = "UTF-8", na.strings = "", header = TRUE)
  
  # format
  id.vars <-  c("region", "sub-region", "transportation_type")
  measure.vars <- which(grepl("^\\d{4}-\\d{2}-\\d{2}$", colnames(a)))
  a <- suppressWarnings(data.table::melt(a, id.vars = id.vars, measure.vars = measure.vars, variable.name = "date"))
  a <- data.table::dcast(a, region + `sub-region` + date ~ transportation_type, value.var = "value")
  
  # date
  a$date <- as.Date(a$date)
  
  # key
  a$key_apple_mobility <- a$region
  idx <- which(!is.na(a$`sub-region`))
  a$key_apple_mobility[idx] <- paste(a$region[idx], a$`sub-region`[idx], sep = ", ")

  # subset
  a <- a[,c("date", "key_apple_mobility", "driving", "transit", "walking")]
  
  # return  
  join(x, a, on = c("date" = "date", "key_apple_mobility" = "key_apple_mobility"))

}
