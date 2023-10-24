date_formats <- c("%Y%m%d", "%y%m%d")
time_formats <- c("%H%M%S", "%H%M")

datetime_formats <- expand.grid(date_formats, time_formats) %>%
  mutate(DateTimeFormat = paste(Var1, Var2)) %>%
  .[["DateTimeFormat"]]

date_parser <- function(vec){
  lubridate::parse_date_time2(vec, orders=date_formats)
}

date_time_parser <- function(vec){
  lubridate::parse_date_time2(vec, orders=datetime_formats)
}


where_list <- list(
  numeric = where(is.numeric),
  date = where(lubridate::is.Date),
  datetime = where(lubridate::is.POSIXct),
  character = where(is.character),
  factor = where(is.factor)
)

quiet_as_numeric <- function(...){
  suppressWarnings(
    as.numeric(...)
  )
}

null_parser <- function(vec){
  .l <- length(vec)
  rep(NA, .l)
}

parse_list  <- list(
  numeric = quiet_as_numeric,
  date = date_parser,
  datetime = date_time_parser,
  character = null_parser,
  factor = null_parser
)


TypedDataFrame2 <- R6Class(
  "TypedDataFrame2",
  list(
    types = NA,
    parsed_data_frames = list(),
    #parsed.columns = list(),
    parse_list = NA,
    where_list = NA,

    data = NA,
    character_data = NA,

    initialize = function(data,
                          .where_list = where_list,
                          .parse_list = parse_list){
      self$data = data
      self$character_data <- select(data, where(is.character))

      self$where_list = .where_list
      self$parse_list = .parse_list

      self$types = names(.where_list)

      for (data_type in self$types){
        self$parsed_data_frames[[data_type]] <- self$get_type(data_type)
      }
    },
    get_type = function(data_type){
      where_f <- self$where_list[[data_type]]
      parse_f <- self$parse_list[[data_type]]

      where_data <- self$data |>
        select(eval(where_f))

      parse_data <- self$character_data %>%
        mutate(across(everything(), parse_f)) %>%
        select(where(not.all.missing))

      colnames(parse_data) <- glue::glue("{colnames(parse_data)} [Parsed]")

      frm <- cbind(where_data, parse_data)

      .class <- paste(data_type, "data.frame", sep=".")

      class(frm) <- c(.class, class(frm))

      frm
    },
    get.data = function(type="numeric"){
      self$parsed_data_frames[[type]]
    },
    get.cols = function(type="numeric"){
      colnames(self$parsed_data_frames[[type]])
    },
    summarize_parsed_frames = function(func=summarize_typed_frame){
      .lis <- lapply(
        self$parsed_data_frames,
        func
      )
      names(.lis) <- names(self$parsed_data_frames)
      .lis
    }
  )
)
# tdf2 <- TypedDataFrame2$new(iris)
# parsed_ratios(tdf2)
# ?across

# devtools::load_all()
#
# data <- frm
# character_data <- data %>% select(where(is.character))
#
# data_type <- "date"
#
# where_f <- where_list[[data_type]]
# parse_f <- parse_list[[data_type]]
#
# where_data <- data |>
#   select(eval(where_f))
#
# parse_data <- character_data %>%
#   mutate(across(everything(), parse_f))  %>%
#   select(where(not.all.missing))
# parse_data
# # frm <- cbind(where_data, parse_data)
#
# .class <- paste(data_type, "data.frame", sep=".")
#
# class(frm) <- c(.class, class(frm))
#

# get.by.parse.default
#
# name.parse
#
# tdf$summarize_parsed_frames


