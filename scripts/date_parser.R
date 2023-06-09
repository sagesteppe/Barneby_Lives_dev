#' create herbarium format dates for specimens
#' 
#' this function will return up to two additional columns with x. 'coll_date_text' a text format for the date of collection and 'det_date_text" a shorter text format date for identification
#' @param x an (sf/tibble/) data frame with both the collection date
#' and determination date
#' @param coll_date date of collection, expected to be of the format 'MM/DD/YYYY', minor checks for compliance to the format will be carried out before returning an error.
#' @param det_date date of determination, same format and processes as above. 
#' @returns original data frame plus: x_dmy, x_day, x_month, x_year, x_text, det_date_text, columns for both parameters which are supplied as inputs
#' @param example
#' @param export
date_parser <- function(x, coll_date, det_date){
  
  coll_date_q <- enquo(coll_date)
  det_date_q <- enquo(det_date)
  
  names_v <- c('_dmy', '_day', '_mo',  '_yr', '_text')
  if(missing(det_date)){
    column_names <- c(coll_date, paste0(coll_date, names_v))
  } else {
    column_names <- c(coll_date, 
                      paste0(coll_date, names_v), 
                        det_date, 
                        paste0(det_date, names_v))
  }
  
  x_dmy <- x |> 
    mutate(across(.cols = c(!!coll_date_q, !!det_date_q), lubridate::mdy, .names = "{.col}_dmy")) |>
    mutate(across(ends_with('_dmy'), ~ lubridate::month(.), .names = "{.col}_mo"),
           across(ends_with('_dmy'), ~ lubridate::day(.), .names = "{.col}_day"),
           across(ends_with('_dmy'), ~ lubridate::year(.), .names = "{.col}_yr")) |>
    mutate(across(.cols = c(!!coll_date_q, !!det_date_q), date2text, .names = '{.col}_text')) |>
    rename_with( ~ stringr::str_remove(., '_dmy'), matches("_dmy_.*$")) |>
    relocate(any_of(column_names), .before = geometry)
  
  return(x_dmy)
}

