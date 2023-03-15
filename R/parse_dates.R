na_or.f <- function(.x, .y){
  #' @importFrom glue glue
  if (is.na(.y)) return(.x)

  if (is.na(.x)) return (.y)

  warning(glue("Multiple matching formats for: \\
                     '{.x}' and '{.y}' - kept '{.x}'"))
  return(.x)
}

na_or <- Vectorize(na_or.f, c(".x", ".y"),
                     USE.NAMES = FALSE, SIMPLIFY = T)

na_reduce <- function(.x, .y){
  dlist <- na_or(.x, .y)
  do.call(c, dlist)
}

DATE.FORMATS <- c("%y-%m-%d", "%Y-%m-%d",
                  "%y/%m/%d", "%Y/%m/%d",
                  "%y%m%d", "%Y%m%d")

DATE.TIME.FORMATS <- paste(DATE.FORMATS, "%H:%M:%S")

date_function <- function(vec){
  UseMethod("date_function")
}

date_function.default <- function(vec){
  #' @exportS3Method
  rep(NA, length(vec))
}

date_function.Date <- function(vec){
  #' @exportS3Method
  vec
}

date_function.POSIXt <- function(vec){
  #' @exportS3Method
  as.Date(vec)
}

make.date.parse <- function(frmt){
  function(vec){
    if (is.character(vec)){
      return(lubridate::fast_strptime(vec, frmt))
    }

    return(rep(NA, length(vec)))
  }
}

parse_dates <- purrr::map(DATE.FORMATS, make.date.parse)
parse_dates <- append(parse_dates, list("None" = date_function))

parse_datetimes <- function(data, date.time.formats=DATE.TIME.FORMATS){
  .lis <- lapply(
    date.time.formats,
    function(i) strptime(data, i)
  )
  reduce(.lis, na_reduce)
}


datetime_function <- function(vec){
  UseMethod("datetime_function")
}

datetime_function.default <- function(vec){
  #' @exportS3Method
  rep(NA, length(vec))
}

datetime_function.POSIXt <- function(vec){
  #' @exportS3Method
  return(vec)
}

parse_datetimes <- purrr::map(DATE.TIME.FORMATS, make.date.parse)
parse_datetimes <- append(parse_datetimes, list("None" = datetime_function))
