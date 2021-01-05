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
#' @param raw logical. Skip data cleaning? Default \code{TRUE}. See details.
#' @param wb character vector of \href{https://data.worldbank.org}{World Bank} indicator codes. See details.
#' @param gmr url to the \href{https://www.google.com/covid19/mobility/}{Google Mobility Report} dataset. See details.
#' @param amr url to the \href{https://covid19.apple.com/mobility}{Apple Mobility Report} dataset. See details.
#' @param cache logical. Memory caching? Significantly improves performance on successive calls. Default \code{TRUE}.
#' @param verbose logical. Print data sources? Default \code{TRUE}. 
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
#' # Data sources
#' s <- covid19cite(x)
#' View(s)
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
#' @export
#'
covid19 <- function(country = NULL,
                    level   = 1,
                    start   = "2010-01-01",
                    end     = Sys.Date(),
                    raw     = TRUE,
                    vintage = FALSE,
                    verbose = TRUE,
                    cache   = TRUE,
                    wb      = NULL,
                    gmr     = NULL,
                    amr     = NULL){

  # fallback
  if(!(level %in% 1:3))
    stop("valid options for 'level' are:
         1: admin area level 1
         2: admin area level 2
         3: admin area level 3")

  # cache
  cachekey <- make.names(sprintf("covid19_%s_%s_%s_%s_%s_%s", level, ifelse(vintage, end, 0), raw, ifelse(is.null(wb),"",paste(wb, collapse = "")), ifelse(is.null(gmr),"",gmr), ifelse(is.null(amr),"",amr)))
  if(cache & exists(cachekey, envir = cachedata))
    return(filter(get(cachekey, envir = cachedata), country = country, start = start, end = end))

  # data
  x    <- data.frame()
  url  <- "https://storage.covid19datahub.io"
  name <- sprintf("%sdata-%s", ifelse(raw, 'raw', ''), level)
  
  # latest
  if(!vintage){
    
    zip  <- sprintf("%s/%s.zip", url, name) 
    file <- sprintf("%s.csv", name) 
    
    x   <- read.zip(zip, file, cache = cache)[[1]]
    src <- read.csv(sprintf("%s/src.csv", url), cache = cache)
    
  }
  # vintage
  else {
    
    if(end < "2020-04-14")
      stop("vintage data not available before 2020-04-14")
    if(end > Sys.Date()-2)
      stop(sprintf("vintage data not available on %s", end))
    
    zip          <- sprintf("%s/%s.zip", url, end)
    files        <- c(paste0("data-",1:3,".csv"), paste0("rawdata-",1:3,".csv"), "src.csv")
    names(files) <- gsub("\\.csv$", "", files)
    
    x <- read.zip(zip, files, cache = cache)
    
    src <- x[["src"]]
    x   <- x[[name]]
    
  }
  
  # check
  if(nrow(x)==0)
    return(NULL)
  
  # date
  x$date <- as.Date(x$date)
  
  # world bank
  if(!is.null(wb))
    x <- worldbank(x, indicator = wb, start = start, end = end)
  
  # google mobility
  if(!is.null(gmr))
    x <- google(x, level = level, url = gmr, cache = cache)
  
  # apple mobility
  if(!is.null(amr))
    x <- apple(x, level = level, url = amr, cache = cache)
  
  # group and order
  x <- x %>%
    dplyr::group_by_at("id") %>%
    dplyr::arrange_at(c("id", "date"))

  # src
  attr(x, "src") <- try(cite(x, src))
  
  # cache
  if(cache)
    assign(cachekey, x, envir = cachedata)

  # verbose
  if(verbose){
    cat("We have invested a lot of time and effort in creating COVID-19 Data Hub, please cite the following when using it:\n")
    print(utils::citation("COVID19"))
    cat("To retrieve citation and metadata of the data sources see ?covid19cite. To hide this message use 'verbose = FALSE'.\n")
  }
  
  # return
  return(filter(x, country = country, start = start, end = end))

}
