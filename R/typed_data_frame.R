typed_summary_metrics <- function(data, func=summarize_typed_frame){
  #' Produce a list of data-type specific variable summaries.
  #'
  #' @examples
  #' typed_summary_metrics(iris)
  #'
  #' @export
  tdf <- TypedDataFrame2$new(data)
  tdf$summarize_parsed_frames(summarize_typed_frame)
}

parse_funcs <- list("numeric" = as.numeric,
                 "date" = parse_dates,
                 "datetime" = parse_datetimes,
                 "character" = parse_character,
                 "factor" = parse_factor
                 )


TypedDataFrame <- R6Class("TypedDataFrame", list(
  #' @importFrom purrr map2
  types = NA,
  parsed.data.frames = list(),
  parsed.columns = list(),
  parse_funcs = NA,
  initialize = function(data, .parse_funcs=parse_funcs){
    self$parsed.data.frames[["all"]] <- data

    self$parse_funcs = .parse_funcs

    for (parse_name in names(self$parse_funcs)){
      parser <- self$parse_funcs[[parse_name]]

      self$parsed.data.frames[[parse_name]] <-
        get.by.parse(data, parser, parse_name)
    }

    self$types <- names(self$parsed.data.frame)
  },
  get.data = function(type="numeric"){
    self$parsed.data.frames[[type]]
  },
  get.cols = function(type="numeric"){
    colnames(self$parsed.data.frames[[type]])
  },
  summarize_parsed_frames = function(func=summarize_typed_frame){
    .lis <- lapply(
      self$parsed.data.frames,
      func
    )
    names(.lis) <- names(self$parsed.data.frames)
    .lis
  }
))

name.parse <- function(frm, .class){
  if (!is.null(.class)){
    .class <- paste(.class, "data.frame", sep=".")
    class(frm) <- prepend(class(frm), .class)
  }

  frm
}

get.by.parse <- function(data, parse, .class=NULL){

  UseMethod("get.by.parse", parse)

}

get.by.parse.default <- function(data, parse, .class=NULL){
  #' @exportS3Method
  #' @importFrom magrittr %>%
  #' @importFrom dplyr mutate select across cur_column
  #' @importFrom tidyselect where all_of
  #' @importFrom purrr prepend reduce
  frm <- data %>%
    dplyr::mutate(across(.fns = parse, .names = "{.col}")) %>%
    select(where(not.all.missing))

  name.parse(frm, .class)
}

get.by.parse.list <- function(data, parse, .class=NULL){
  #' @exportS3Method
  .lis <- map(parse, function(i) get.by.parse(data, i, .class), .progress=T)

  bindfrm <- do.call(cbind, .lis)

  colnames(bindfrm) <- unlist(
    map2(.lis, names(.lis), ~ glue("{colnames(.x)} [parser: {.y}]"))
  )

  bindfrm

  name.parse(bindfrm, .class)
}

remainder.frame <- function(data, parsed.data.frames){
  .cols <- colnames(data)

  .lis <- lapply(
    parsed.data.frames,
    colnames
  )

  used.cols <- unique(unlist(.lis))

  unused.cols <- .cols[!.cols %in% used.cols]
  select(data, unused.cols)
}
