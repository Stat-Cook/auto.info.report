is.boolean <- function(vec){
  all(vec == TRUE | vec == FALSE)
}

where_any <- function(...){
  .list <- list(...)

  where.list <- map(.list, where)

  function(x, ...)  {
    bools <- map(where.list, \(.fn) .fn(x, ...))
    reduce(bools, or)
  }
}

#' @importFrom tidyselect where
#' @importFrom lubridate is.Date is.POSIXct


select_numerics <- where(is.numeric)
#' @export

select_booleans <- where(is.boolean)
#' @export

select_dates <- where(lubridate::is.Date)
#' @export

select_POSIXct <- where(lubridate::is.POSIXct)
#' @export

select_character <- where(is.character)
#' @export

select_factor <- where(is.factor)
#' @export

select_categorical <- where_any(is.factor, is.character)
#' @export


