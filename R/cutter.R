methods <- list(
  "equalwidth" = function(data, n)  seq(min(data), max(data), length.out=n+1),
  "equalfreq" = function(data, n) quantile(data, seq(0, 1, 1/n))
)

extend_to_inf <- function(vec){
  vec[1] <- -Inf
  vec[length(vec)] <- Inf
  vec
}

#' @importFrom glue glue
Cutter <- R6Class("Cutter", list(
  cuts = NULL,
  learn = function(data, n=5, method="equalfreq"){
    if (!method %in% names(methods)){
      stop(glue("Method '{method}' not implemented."))
    }

    non.na.data <- data[!is.na(data)]

    method.f <- methods[[method]]

    cuts <- method.f(non.na.data, n)
    self$cuts <- extend_to_inf(cuts)

    self$transform(data)
  },
  transform = function(data){
    #' @importFrom Hmisc cut2
    if (is.null(self$cuts)){
      self$learn(data)
    }
    cut2(data, cuts = self$cuts)
  }
))


