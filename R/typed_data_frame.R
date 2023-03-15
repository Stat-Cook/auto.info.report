typed_summary_metrics <- function(data, func=summarize_typed_frame){
  #' @export
  tdf <- TypedDataFrame$new(data)
  tdf$summarize_parsed_frames(summarize_typed_frame)
}

parse_funcs <- c("numeric" = as.numeric,
                 "date" = parse_dates,
                 "datatime" = parse_datetimes,
                 "character" = parse_character,
                 "factor" = parse_factor
                 )

TypedDataFrame <- R6Class("TypedDataFrame", list(
  types = NA,
  parsed.data.frames = list(),
  parsed.columns = list(),
  parse_funcs = NA,
  initialize = function(data, .parse_funcs=parse_funcs){
    self$parsed.data.frames[["all"]] <- data

    self$parse_funcs = .parse_funcs

    for (parse_name in names(self$parse_funcs)){
      parser <- self$parse_funcs[parse_name]

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

get.by.parse <- function(data, parse, .class=NULL){

  frm <- UseMethod("get.by.parse", parse)

  if (!is.null(.class)){
    .class <- paste(.class, "data.frame", sep=".")
    class(frm) <- prepend(class(frm), .class)
  }

  frm
}

get.by.parse.default <- function(data, parse, .class=NULL){
  #' @importFrom magrittr %>%
  #' @importFrom dplyr mutate select across cur_column
  #' @importFrom tidyselect where all_of
  #' @importFrom purrr prepend reduce
  frm <- data %>%
    dplyr::mutate(across(.fns = parse, .names = "{.col}")) %>%
    select(where(not.all.missing))


  frm
}


get.by.parse.list <- function(data, parse, .class=NULL){
  .lis <- map(parse, function(i) get.by.parse(data, i, .class), .progress=T)

  bindfrm <- do.call(cbind, .lis)
  colnames(bindfrm) <- unlist(
    map2(.lis, names(.lis), ~ glue("{colnames(.x)} [parser: {.y}]"))
  )

  bindfrm
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
