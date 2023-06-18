#' take mdy format date and make it written herbarium label format
#' 
#' @description This will ensure that dates are formatted in a manner which makes
#' them readily interpretable to researchers worldwide.
#' @param x a data frame with dates
#' @param example 
#' first50dates <- paste0(sample(3:9, size = 50, replace = T), '-', 
#'    sample(1:29, size = 50, replace  = T), '-', 
#'    rep(2023, times = 50 ))
#'    
#' head(first50dates)
#' first50dates <- date2text(first50dates)
#' head(first50dates)
#' @param export
date2text <- function(x) {
  
  x1 <- lubridate::mdy(x)
  
  text <- paste0(lubridate::day(x1), # just grab day of month here
                 ' ',
                 lubridate::month(x1, abbr = T, label = T), # grab the abbreviation for the month here
                 ', ',
                 lubridate::year(x1)) # grab the year here
  return(text)
}
