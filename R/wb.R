worldbank <- function(x, indicator, start, end){
  
  # date
  start <- format(as.Date(start), "%Y")
  end   <- format(as.Date(end), "%Y")
  
  # download
  if(exists('wb_data', where = asNamespace('wbstats'), mode='function')){
    
    w <- wbstats::wb_data(indicator = indicator, start_date = start, end_date = end, return_wide = FALSE)
    w <- w %>% tidyr::pivot_wider(c('iso3c','date'), names_from = 'indicator_id', values_from = 'value')
    
  }
  else{
    
    w <- wbstats::wb(indicator = indicator, startdate = start, enddate = end)
    w <- w %>% tidyr::pivot_wider(c('iso3c','date'), names_from = 'indicatorID', values_from = 'value')
    
  }
  
  # column names
  map         <- c("iso_alpha_3" = "iso3c", "date" = "date", indicator)
  from        <- unname(map)
  to          <- names(map)
  miss        <- to == ""
  to[miss]    <- from[miss]
  colnames(w) <- sapply(colnames(w), function(x) to[which(from==x)])

  # get most recent value for each country
  w <- w %>% 
    
    dplyr::group_by_at('iso_alpha_3') %>% 
    
    dplyr::arrange_at('date') %>%
    
    dplyr::group_map(function(x, iso) {
      
      iso <- iso[[1]]
      
      idx <- ifelse(is.na(x), 1L, row(x))
      idx <- apply(idx, 2, max)
      
      x <- sapply(1:ncol(x), function(i) x[idx[i], i]) 
      x$iso_alpha_3 <- iso
      
      return(x)
    
    }) %>%
    
    dplyr::bind_rows()
  
  # drop year
  w$date <- NULL
  
  # merge
  x <- merge(x, w, by = 'iso_alpha_3', all.x = TRUE)
  
  # return
  return(x)
  
}

