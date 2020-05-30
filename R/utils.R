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

cite <- function(x, src, verbose){
  
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
  
  if(verbose){
    
    y <- x %>% 
      dplyr::mutate(url = gsub("(http://|https://|www\\.)([^/]+)(.*)", "\\1\\2", url)) %>%
      dplyr::distinct_at(c('title', 'url'), .keep_all = TRUE)
    
    y <- apply(y, 1, function(y){
      
      textVersion <- y['textVersion']
      if(is.na(textVersion))
        textVersion <- paste0(y['title'],' (',y['year'],')',', ',y['url'])
      
      utils::bibentry(
        bibtype     = ifelse(is.na(y['bibtype']), "Misc", y['bibtype']),
        title       = y['title'],
        year        = y['year'],
        author      = y['author'],
        institution = y['institution'], 
        textVersion = textVersion
      )  
      
    })
    
    cit <- utils::citation("COVID19")
    for(i in 1:length(y))
      cit <- c(y[[i]], cit)
    
    print(cit, style = "citation")
    cat("To hide the data sources use 'verbose = FALSE'.")
    
  }
  
  return(x)
  
}

