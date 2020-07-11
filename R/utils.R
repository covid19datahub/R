#' @importFrom dplyr %>%
NULL

cachecall <- function(fun, ...){
  
  args  <- list(...)
  cache <- ifelse(is.null(args$cache), TRUE, args$cache)
  key   <- make.names(sprintf("%s_%s",paste(deparse(fun), collapse = ''),paste(names(args),args,sep = ".",collapse = "..")))
  
  if(cache & exists(key, envir = cachedata))
    return(get(key, envir = cachedata))
  else
    x <- do.call(fun, args = args)
  
  if(cache)
    assign(key, x, envir = cachedata)
  
  return(x)
  
}

read.csv <- function(file, cache, na.strings = "", stringsAsFactors = FALSE, encoding = "UTF-8", ...){
  
  if(cache)
    x <- cachecall(utils::read.csv, file = file, na.strings = na.strings, stringsAsFactors = stringsAsFactors, encoding = encoding, ...)
  else
    x <- utils::read.csv(file = file, na.strings = na.strings, stringsAsFactors = stringsAsFactors, encoding = encoding, ...)
  
  return(x)
  
}

read.zip <- function(zip, files, cache, ...){
  
  read.zip <- function(zip, files, ...){
    
    temp <- tempfile()
    utils::download.file(zip, temp, quiet = TRUE)  
    
    lapply(files, function(file){
      
      read.csv(unz(temp, file), cache = FALSE, ...)
      
    })
    
  }
  
  if(cache)
    x <- cachecall(read.zip, zip = zip, files = files, ...)
  else 
    x <- read.zip(zip = zip, files = files, ...)
  
  return(x)
  
}

filter <- function(x, country, start, end){
  
  src <- attr(x, 'src')
  x   <- x[x$date >= start & x$date <= end,]
  
  if(length(country <- toupper(country)) > 0){
    
    id <- iso_alpha_3 <- iso_alpha_2 <- iso_numeric <- administrative_area_level_1 <- NA
    x  <- dplyr::filter(x, toupper(id) %in% country | iso_alpha_3 %in% country | iso_alpha_2 %in% country | iso_numeric %in% country | toupper(administrative_area_level_1) %in% country)
    
  }
  
  if(!is.null(src))
    attr(x, 'src') <- src[src$iso_alpha_3 %in% x$iso_alpha_3,]
  
  return(x)
  
}

cite <- function(x, src){
  
  x <- x %>% 
    
    dplyr::group_by_at('iso_alpha_3') %>%
    
    dplyr::group_map(function(x, iso){
      
      iso   <- iso[[1]]
      level <- unique(x$administrative_area_level)
      
      var   <- apply(x, 2, function(x) !all(is.na(x) | x==0))
      var   <- names(var)[var]
      
      s     <- src[which(src$data_type %in% var & src$iso_alpha_3==iso & src$administrative_area_level==level),]
      var   <- var[!(var %in% s$data_type)]
      
      if(length(var)>0)
        s <- s %>% 
          dplyr::bind_rows(src[which(src$data_type %in% var & is.na(src$iso_alpha_3) & is.na(src$administrative_area_level)),])
        
      s$iso_alpha_3 <- iso
      s$administrative_area_level <- level  
      
      return(s)
      
    }) %>%
    
    dplyr::bind_rows() %>%
    
    dplyr::distinct()
  
  return(x)
  
}

