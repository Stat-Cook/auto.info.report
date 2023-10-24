is.not.na <- function(vec){
  !is.na(vec)
}

not.na.ratios <- function(frame){
  frame %>%
    mutate(across(everything(), is.not.na)) %>%
    dplyr::summarize(across(everything(), mean))
}

remove_parser <- function(frm){
  #' @importFrom stringr str_replace
  .cols <- colnames(frm)
  colnames(frm) <- str_replace(.cols, " \\[parser:.*\\]", "")
  frm
}


parsed_ratios <- function(object){
  #' Summarize how much of each variable fits within a defined data type.
  #'
  #' @examples
  #' parsed_ratios(iris)
  #'
  #' @export
  UseMethod("parsed_ratios")
}

parsed_ratios.data.frame <- function(object){
   #' @exportS3Method
  tdf <- TypedDataFrame2$new(object)
  parsed_ratios(tdf)
}


parsed_ratios.TypedDataFrame <- function(object){
  #' @exportS3Method
  not.na.frms <- lapply(object$parsed.data.frames,
                        not.na.ratios) |>
    lapply(remove_parser) |>
    do.call(what=plyr::rbind.fill)

  rownames(not.na.frms) <- names(object$parsed.data.frames)

  not.na.frms[is.na(not.na.frms)] <- 0
  100 * not.na.frms
}


parsed_ratios.TypedDataFrame2 <- function(object){
  #' @exportS3Method
  not.na.frms <- lapply(object$parsed_data_frames,
                        not.na.ratios) |>
    lapply(remove_parser) |>
    do.call(what=plyr::rbind.fill)

  rownames(not.na.frms) <- names(object$parsed_data_frames)

  not.na.frms[is.na(not.na.frms)] <- 0
  100 * not.na.frms
}
