#' COVID-19 Data Hub Data Sources
#'
#' Retrieve citation and metadata of the data sources.
#' 
#' @param x dataset returned by \code{\link{covid19}}.
#' @param verbose logical. Print citation? Default \code{TRUE}. 
#'
#' @return \code{data.frame} of data sources.
#'
#' @examples
#' \dontrun{
#'
#' # Download data 
#' x <- covid19("USA")
#' 
#' # Data Sources
#' s <- covid19cite(x)
#' View(s)
#' }
#'
#' @export
#'
covid19cite <- function(x, verbose = TRUE){

  x <- attr(x, 'src')
  if(is.null(x))
    stop("No metadata found. Make sure 'x' is a dataset returned by the covid19() function.")
  
  if(verbose){
    
    y <- x %>%
      dplyr::distinct_at(c('title'), .keep_all = TRUE)
    
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
    
    for(i in 1:length(y)){
      if(i==1)
        cit <- y[[i]]
      else
        cit <- c(y[[i]], cit)
    }
      
    print(cit, style = "citation")
    
  }
  
  return(x)
  
}