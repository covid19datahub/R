worldbank <- function(x, indicator, start, end){
  
  # check
  if(!requireNamespace("wbstats", quietly = TRUE))
    stop("Package 'wbstats' needed but not installed. Install with: install.packages('wbstats')")
  
  # date
  start <- format(as.Date(start), "%Y")
  end   <- format(as.Date(end), "%Y")
  
  # download
  w <- wbstats::wb_data(indicator = indicator, start_date = start, end_date = end, return_wide = FALSE)
  
  # convert to data.table and sort
  w <- data.table::data.table(w)
  data.table::setkey(w, iso3c, date)
  
  # bind variables for CRAN
  value <- iso3c <- indicator_id <- NULL
  # fill most recent value
  w[, value := data.table::nafill(value, type = "locf"), by = list(iso3c, indicator_id)]
  
  # pivot wider
  w <- data.table::dcast(w, iso3c + date ~ indicator_id, value.var = "value", fill = NA)
  
  # get most recent value for each country
  w <- unique(w, by = "iso3c", fromLast = TRUE)  
    
  # drop year
  w$date <- NULL
  
  # rename
  data.table::setnames(w, old = indicator, new = names(indicator))
  
  # return
  join(x, w, on = c("iso_alpha_3" = "iso3c"))
  
}
