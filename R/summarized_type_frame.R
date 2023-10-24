summarize_typed_frame <- function(data, ...){
  #' Produce variable summaries depending on frame data type
  #'
  #' @examples
  #' tdf2 <- TypedDataFrame2$new(iris)
  #' summarize_typed_frame(tdf2$parsed_data_frames[["numeric"]])
  #'
  #' @export
  if (ncol(data) == 0){
    return(data.frame())
  }
  UseMethod("summarize_typed_frame", data)
}

dtype <- function(vec){
  class(vec)[1]
}

mean.missing <- function(vec){
  miss.vec <- missing(vec)
  mean(miss.vec)
}



summarize_typed_frame.default <- function(data, ...){
  #' @export
  frm <- data.frame(
    Cardinality = sapply(data, cardinality),
    Dtype = sapply(data, dtype),
    `% Missing` = round(100*sapply(data, mean.missing), 1)
  )
  colnames(frm) <- c("Cardinality", "Dtype", "% Missing")
  frm
}

summarize_typed_frame.numeric.data.frame <- function(data, ...){
  #' @importFrom stats median
  #' @exportS3Method
  # specific.metrics <- data.frame(
  #   Mean = sapply(data, mean),
  #   Median = sapply(data, median),
  #   Min = sapply(data, min),
  #   Max = sapply(data, max)
  # )
  # colnames(specific.metrics) <- c("Mean", "Median", "Min", "Max")

  data <- as_tibble(data)

  specific.metrics <- rbind(
    Mean = summarize(data, across(everything(), mean, na.rm=T)),
    Median = summarize(data, across(everything(), median, na.rm=T)),
    Min = summarize(data, across(everything(), min, na.rm=T)),
    Max = summarize(data, across(everything(), max, na.rm=T))
  )  %>% t()

  default.metrics <- summarize_typed_frame.default(data)

  cbind(specific.metrics, default.metrics)
}


summarize_typed_frame.date.data.frame <- function(data, ...){
  #' @exportS3Method
  specific.metrics <- data.frame(
    ID = colnames(data)
  )
  colnames(specific.metrics) <- c("ID")

  numeric.metrics <- data %>%
    summarize_typed_frame.numeric.data.frame(data)

  cbind(specific.metrics, numeric.metrics)
}

summarize_typed_frame.datetime.data.frame <- function(data, ...){
  #' @exportS3Method
  specific.metrics <- data.frame(
    ID = colnames(data)
  )
  colnames(specific.metrics) <- c("ID")

  numeric.metrics <- summarize_typed_frame.numeric.data.frame(data)

  cbind(specific.metrics, numeric.metrics)
}
