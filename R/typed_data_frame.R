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


    # self$parsed.data.frames[["date"]] <-
    #   get.by.parse(data, parse_dates, "date")
    # self$parsed.data.frames[["datetime"]] <-
    #   get.by.parse(data, parse_datetimes, "datetime")
    # self$parsed.data.frames[["character"]] <-
    #   get.by.parse(data, parse_characters, "character")
    # self$parsed.data.frames[["factors"]] <-
    #   get.by.parse(data, parse_characters, "factors")

    # self$parsed.data.frames[["other"]] <- remainder.frame(data, self$parsed.data.frame)

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
  #' @importFrom magrittr %>%
  #' @importFrom dplyr mutate select across cur_column
  #' @importFrom tidyselect where all_of
  #' @importFrom purrr prepend reduce
  frm <- data %>%
    dplyr::mutate(across(.fns = parse, .names = "{.col}")) %>%
    select(where(not.all.missing))

  if (!is.null(.class)){
    .class <- paste(.class, "data.frame", sep=".")
    class(frm) <- prepend(class(frm), .class)
  }
  frm
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


#
# df <- data.frame(
#   A = sample(letters, 400, T),
#   B = sample(LETTERS, 400, T),
#   C = rnorm(400),
#   D = c(sample(letters, 200, T), rnorm(200)),
#   E = factor(sample(letters, 400, T))
# )
#
# tdf <- TypedDataFrame$new(df)
# tdf$parsed.data.frames[["character"]]
#