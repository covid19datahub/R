#' COVID-19 Data Hub
#'
#' Download COVID-19 data across governmental sources at national, regional, and city level.
#' Includes the time series of vaccines, tests, cases, deaths, recovered, hospitalizations, intensive therapy, 
#' and policy measures by \href{https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker}{Oxford COVID-19 Government Response Tracker}.
#' Provides a seamless integration with 
#' \href{https://data.worldbank.org/}{World Bank Open Data}, 
#' \href{https://www.google.com/covid19/mobility/}{Google Mobility Reports}, 
#' \href{https://covid19.apple.com/mobility}{Apple Mobility Reports}.
#'
#' @param country vector of country names or \href{https://github.com/covid19datahub/COVID19/blob/master/inst/extdata/db/ISO.csv}{ISO codes} (alpha-2, alpha-3 or numeric).
#' @param level integer. Granularity level. 1: country-level data. 2: state-level data. 3: lower-level data.
#' @param start the start date of the period of interest.
#' @param end the end date of the period of interest.
#' @param vintage logical. Retrieve the snapshot of the dataset that was generated at the \code{end} date instead of using the latest version. Default \code{FALSE}.
#' @param wb character vector of \href{https://data.worldbank.org}{World Bank} indicator codes. See details.
#' @param gmr url to the \href{https://www.google.com/covid19/mobility/}{Google Mobility Report} dataset. See details.
#' @param amr url to the \href{https://covid19.apple.com/mobility}{Apple Mobility Report} dataset. See details.
#' @param dir folder to store downloads.
#' @param verbose logical. Print data sources? Default \code{TRUE}. 
#' @param ... not used.
#'
#' @details 
#' If \code{raw=FALSE}, the raw data are cleaned by filling missing dates with \code{NA} values. 
#' This ensures that all locations share the same grid of dates and no single day is skipped. 
#' Then, \code{NA} values are replaced with the previous non-\code{NA} value or \code{0}.
#' 
#' The dataset can be extended with \href{https://data.worldbank.org}{World Bank Open Data} via the argument \code{wb}, a character vector of indicator codes.
#' The codes can be found by inspecting the corresponding URL. For example, the code of the GDP indicator available at \url{https://data.worldbank.org/indicator/NY.GDP.MKTP.CD} is \code{NY.GDP.MKTP.CD}.
#' The latest data available between the \code{start} and \code{end} date are downloaded.
#'
#' The dataset can be extended with \href{https://www.google.com/covid19/mobility/}{Google Mobility Reports} via the argument \code{gmr}, the url to the Google CSV file.
#' At the time of writing, the CSV is available \href{https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv}{here}. 
#' 
#' The dataset can be extended with \href{https://covid19.apple.com/mobility}{Apple Mobility Reports} via the argument \code{amr}, the url to the Apple CSV file.
#' At the time of writing, the CSV is available \href{https://covid19-static.cdn-apple.com/covid19-mobility-data/2023HotfixDev26/v3/en-us/applemobilitytrends-2021-01-01.csv}{here}.
#'
#' Refer to \href{https://covid19datahub.io/articles/data.html}{this webpage} for more details on the data.
#'
#' @return Grouped \code{tibble} (\code{data.frame}). See the \href{https://covid19datahub.io/articles/doc/data.html}{dataset documentation}
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
#' # Specific country data by city
#' x <- covid19(c("Italy","US"), level = 3)
#' 
#' # Merge with World Bank data. It may take some time...
#' wb <- c("gdp" = "NY.GDP.MKTP.CD", "hosp_beds" = "SH.MED.BEDS.ZS")
#' x  <- covid19(wb = wb)
#' 
#' # Merge with Google Mobility Reports. It may take some time...
#' gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
#' x   <- covid19(gmr = gmr)
#' 
#' # Merge with Apple Mobility Reports. It may take some time...
#' amr <- "https://covid19-static.cdn-apple.com/covid19-mobility-data/"
#' amr <- paste0(amr, "2023HotfixDev26/v3/en-us/applemobilitytrends-2021-01-01.csv")
#' x   <- covid19(amr = amr)
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
    
    url <- sprintf("%s/%s%s", baseurl, vintage, ifelse(vintage>="2021-11-13", ".db.gz", ".zip"))
    ext <- tools::file_ext(url)
    if(ext=="zip"){
      x <- read.zip(url, dir = dir, level = level, verbose = verbose)  
      x <- filter(x, country = country, level = level, start = start, end = end)
    }
    if(ext=="gz"){
      x <- read.db(url, dir = dir, country = country, level = level, start, end, verbose = verbose)
    }
    
  }

  # convert to data.table
  x <- data.table::data.table(x)
  
  # date
  x$date <- as.Date(x$date)
  
  # world bank
  if(!is.null(wb))
    x <- worldbank(x, indicator = wb, start = start, end = end)
  
  # google mobility
  if(!is.null(gmr))
    x <- google(x, level = level, url = gmr, dir = dir, verbose = verbose)
  
  # apple mobility
  if(!is.null(amr))
    x <- apple(x, level = level, url = amr, dir = dir, verbose = verbose)
  
  # verbose
  if(verbose){
    cat("We have invested a lot of time and effort in creating COVID-19 Data Hub, please cite the following when using it:\n")
    print(utils::citation("COVID19"))
    cat("To hide this message use 'verbose = FALSE'.\n")
  }
  
  # return
  data.frame(x)

}
