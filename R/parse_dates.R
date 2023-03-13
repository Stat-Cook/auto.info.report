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

parse_dates <- function(data, date.formats=DATE.FORMATS){
  .lis <- lapply(
    date.formats,
    function(i) strptime(data, i)
  )
  reduce(.lis, na_reduce)
}

parse_datetimes <- function(data, date.time.formats=DATE.TIME.FORMATS){
  .lis <- lapply(
    date.time.formats,
    function(i) strptime(data, i)
  )
  reduce(.lis, na_reduce)
}

