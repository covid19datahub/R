#' COVID-19 Data Hub
#'
#' Download COVID-19 data from \url{https://covid19datahub.io}
#'
#' @param country vector of country names or \href{https://en.wikipedia.org/wiki/List_of_ISO_3166_country_codes}{ISO codes} (ISO 3166-1 Alpha-2 code, Alpha-3 code, or numeric code). By default, downloads data for all countries.
#' @param level integer. Granularity level. 1: country-level data. 2: state-level data. 3: lower-level data.
#' @param start,end the start and the end date of the period of interest. The data are subsetted to match this time range.
#' @param vintage date. This parameter allows to retrieve the snapshot of the dataset that was available on the given date. This typically differs from subsetting the latest data, as most governments are updating the data retroactively. Available since 2020-04-14.
#' @param wb character vector of \href{https://data.worldbank.org}{World Bank} indicator codes. See details.
#' @param gmr link to the \href{https://www.google.com/covid19/mobility/}{Google Mobility Report} dataset, or \code{TRUE}. See details.
#' @param amr link to the \href{https://covid19.apple.com/mobility}{Apple Mobility Report} dataset, or \code{TRUE}. See details.
#' @param dir folder where the data files are to be downloaded. 
#' @param verbose logical. Print on progress? Default \code{TRUE}. 
#' @param ... backward compatibility, not used.
#'
#' @details 
#' 
#' Country-level covariates by \href{https://data.worldbank.org}{World Bank Open Data} can be added via the argument \code{wb}.
#' This is a character vector of indicator codes to download.
#' The codes can be found by inspecting the corresponding URL. 
#' For example, the code of the indicator "Hospital beds (per 1,000 people)" available at \url{https://data.worldbank.org/indicator/SH.MED.BEDS.ZS} is \code{SH.MED.BEDS.ZS}.
#' The indicators are typically available at a yearly frequency. 
#' This function returns the latest data available between the \code{start} and the \code{end} date.
#' See the table at the bottom of \href{https://datatopics.worldbank.org/universal-health-coverage/coronavirus/}{this page} for suggested indicators.
#'
#' Mobility data by \href{https://www.google.com/covid19/mobility/}{Google Mobility Reports} can be added via the argument \code{gmr}.
#' This is the link to the Google "CSV by geographic area" ZIP folder. 
#' At the time of writing, the link is \url{https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip}.
#' As the link has been stable since the beginning of the pandemic, the function accepts \code{gmr=TRUE} to automatically use this link.
#' 
#' Mobility data by \href{https://covid19.apple.com/mobility}{Apple Mobility Reports} can be added via the argument \code{amr}.
#' This is the link to the Apple "All CSV data" file. This link is changing constantly. 
#' Consider downloading the data file from the website first, and then set \code{amr="path/to/file.csv"}.
#' If \code{amr=TRUE} is provided, the function tries to detect the latest URL from \href{https://covid19-static.cdn-apple.com/covid19-mobility-data/current/v3/index.json}{this endpoint}.
#'
#' Refer to \href{https://covid19datahub.io/reference/index.html}{this webpage} for the details on the data sources, and 
#' \href{https://covid19datahub.io/news/index.html}{see the changelog} for the latest news about the dataset.
#'
#' @return \code{data.frame}. See the \href{https://covid19datahub.io/articles/docs.html}{dataset documentation}
#'
#' @examples
#' \dontrun{
#'
#' # Worldwide data by country
#' x <- covid19()
#'
#' # Worldwide data by state
#' x <- covid19(level = 2)
#'
#' # Data for specific countries by county/province
#' x <- covid19(c("Italy", "US"), level = 3)
#' 
#' # Retrieve the data that were available on 15 May, 2020
#' x <- covid19(vintage = "2020-05-15")
#' 
#' # Download the files in the folder "data"
#' dir.create("data")
#' x <- covid19(dir = "data")
#' 
#' # World Bank data
#' wb <- c("gdp" = "NY.GDP.MKTP.CD", "hosp_beds" = "SH.MED.BEDS.ZS")
#' x  <- covid19(wb = wb)
#' 
#' # Google Mobility Reports
#' x <- covid19(gmr = TRUE)
#' 
#' # Apple Mobility Reports
#' # - download the CSV data file from https://covid19.apple.com/mobility
#' # - use the path to the file that you have downloaded
#' x <- covid19(amr = "path/to/file.csv")
#' 
#' }
#'
#' @source \url{https://covid19datahub.io}
#'
#' @references 
#' Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open Source Software 5(51):2376, \doi{10.21105/joss.02376}.
#'
#' @note 
#' We have invested a lot of time and effort in creating \href{https://covid19datahub.io}{COVID-19 Data Hub}, please:
#' 
#' \itemize{
#' \item cite \href{https://joss.theoj.org/papers/10.21105/joss.02376}{Guidotti and Ardia (2020)} when using \href{https://covid19datahub.io}{COVID-19 Data Hub}.
#' \item place the URL \url{https://covid19datahub.io} in a footnote to help others find \href{https://covid19datahub.io}{COVID-19 Data Hub}.
#' \item you assume full risk for the use of \href{https://covid19datahub.io}{COVID-19 Data Hub}. 
#' We try our best to guarantee the data quality and consistency and the continuous filling of the Data Hub. 
#' However, it is free software and comes with ABSOLUTELY NO WARRANTY. 
#' Reliance on \href{https://covid19datahub.io}{COVID-19 Data Hub} for medical guidance or use of \href{https://covid19datahub.io}{COVID-19 Data Hub} in commerce is strictly prohibited.
#' }
#' 
#' @importFrom data.table :=
#' 
#' @export
#'
covid19 <- function(country = NULL,
                    level   = 1,
                    start   = "2010-01-01",
                    end     = Sys.Date(),
                    vintage = NULL,
                    wb      = NULL,
                    gmr     = NULL,
                    amr     = NULL,
                    dir     = tempdir(),
                    verbose = TRUE,
                    ...){

  if(any(!level %in% 1:3))
    stop("'level' must be one of 1, 2, 3 or a combination of those.")

  if(is.logical(vintage)){
    if(!vintage){
      vintage <- NULL
    }
    else{
      vintage <- end
      warning(sprintf("Using vintage='%s' (end date). See ?covid19 for the new usage of the 'vintage' parameter.", vintage))
    }
  }
  
  if(is.null(vintage)){
    
    if(is.null(country) | all(level==1)){
      x <- data.table::rbindlist(fill = TRUE, lapply(level, function(i){
        url <- endpoint("level/", i, ".csv.gz")
        read.gz(url, dir = dir, verbose = verbose)
      }))
      x <- filter(x, country = country, level = level, start = start, end = end)
    }
    
    else{
      url <- endpoint("country/index.csv.gz")
      map <- read.gz(url, dir = dir, verbose = verbose)
      iso <- map$iso_alpha_3[
        map$name %in% country |
        map$iso_alpha_3 %in% country |
        map$iso_alpha_2 %in% country |
        map$iso_numeric %in% country ]
      x <- data.table::rbindlist(fill = TRUE, lapply(iso, function(i){
        url <- endpoint("country/", i, ".csv.gz")
        read.gz(url, dir = dir, verbose = verbose)
      }))
      x <- filter(x, country = country, level = level, start = start, end = end)
    }
    
  }
  
  else{
    
    url <- sprintf("%s/%s%s", baseurl, vintage, ifelse(vintage>="2021-11-15", ".db.gz", ".zip"))
    ext <- tools::file_ext(url)
    if(ext=="zip"){
      x <- read.zip(url, dir = dir, level = level, verbose = verbose)  
      x <- filter(x, country = country, level = level, start = start, end = end)
    }
    if(ext=="gz"){
      x <- read.db(url, dir = dir, country = country, level = level, start, end, verbose = verbose)
    }
    
  }

  x$date <- as.Date(x$date)
  x <- data.table::data.table(x)
  
  if(!is.null(wb))
    x <- worldbank(x, indicator = wb, start = start, end = end)
  
  if(!is.null(gmr))
    x <- google(x, level = level, url = gmr, dir = dir, verbose = verbose)
  
  if(!is.null(amr))
    x <- apple(x, level = level, url = amr, dir = dir, verbose = verbose)
  
  if(verbose){
    cat("We have invested a lot of time and effort in creating COVID-19 Data Hub, please cite the following when using it:\n")
    print(utils::citation("COVID19"))
    cat("To hide this message use 'verbose = FALSE'.\n")
  }
  
  return(data.frame(x))
  
}
