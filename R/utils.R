baseurl <- "https://storage.covid19datahub.io"

endpoint <- function(...){
  ep <- paste0(list(...), collapse = "")
  paste(baseurl, ep, sep = "/")
}

local <- function(..., dir, timestamp){
  file <- paste0(list(...), collapse = "/")
  file <- gsub("^https?://[^/]*/", "", file)
  file <- gsub("\\.(gz|zip)$", "", file)
  if(timestamp) 
    dir <- paste(dir, format(Sys.time(), "%Y-%m-%d"), sep = "/")
  paste(dir, file, sep = "/")
}

# Read GZ files
read.gz <- function(url, dir, verbose){
  file <- download(url, dir = dir, verbose = verbose, timestamp = TRUE)
  data.table::fread(file, showProgress = verbose, encoding = "UTF-8", na.strings = "")
}

# Read vintage ZIP
read.zip <- function(url, dir, level, verbose){
  file <- download(url, dir = dir, verbose = verbose, timestamp = FALSE)
  data.table::rbindlist(fill = TRUE, lapply(level, function(i){
    rawdata <- sprintf("%s/rawdata-%s.csv", file, i)
    dt <- try(data.table::fread(
      rawdata, showProgress = verbose, encoding = "UTF-8", na.strings = ""), 
      silent = !verbose)
    if("try-error" %in% class(dt)) return(NULL)
    return(dt)
  }))
}


read.db <- function(url, dir, country, level, start, end, verbose){
  level <- paste(level, collapse = "','")
  country <- paste(country, collapse = "','")
  sql <- sprintf("
    SELECT * 
    FROM 
      timeseries NATURAL JOIN location 
    WHERE 
      date BETWEEN '%s' AND '%s' AND
      administrative_area_level IN ('%s')", start, end, level)
  if(country!="")
    sql <- sprintf("%s AND (
        administrative_area_level_1 IN ('%s') OR
        iso_alpha_3 IN ('%s') OR
        iso_alpha_2 IN ('%s') OR
        iso_numeric IN ('%s')
      )", sql, country, country, country, country)
  file <- download(url, dir = dir, verbose = verbose, timestamp = FALSE)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), file)
  x <- RSQLite::dbGetQuery(con, sql)
  RSQLite::dbDisconnect(con)
  return(x)
}


download <- function(url, dir, verbose, timestamp){
  
  # Exit if file already exists
  file <- local(url, dir = dir, timestamp = timestamp)
  if(file.exists(file)) 
    return(file)
  
  # Download
  tmp <- tempfile()
  utils::download.file(url, destfile = tmp, mode = "wb", quiet = !verbose)

  # Extract in the output path
  ext <- tools::file_ext(url)
  if(ext=="gz")
    R.utils::gunzip(tmp, file)
  else if(ext=="zip")
    utils::unzip(tmp, exdir = file)
  else
    file <- NULL

  # Return file path
  return(file)
  
}

filter <- function(x, country, level, start, end){
  x <- x[x$date>=start & x$date<=end & x$administrative_area_level %in% level,]
  if(!is.null(country)){
    x <- x[x$administrative_area_level_1 %in% country |
           x$iso_alpha_3 %in% country |
           x$iso_alpha_2 %in% country |
           x$iso_numeric %in% country,]
  }
  return(x)
}

join <- function(x, y, on){
  data.table::setnames(y, old = on, new = names(on))
  data.table::setcolorder(y[x, on = names(on)], unique(names(x), names(y)))
}

