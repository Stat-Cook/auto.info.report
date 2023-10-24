learn_discretizer <- function(vec, n=5, method="equalfreq"){
  cutter <- Cutter$new()
  cutter$learn(vec, n=n, method="equalfreq")
  cutter
}

learn_discretizers <- function(data, n=5, method="equalfreq"){
  .lis <- lapply(data, function(i) learn_discretizer(i, n=n, method=method))
  class(.lis) <- "Discretizer"
  .lis
}

# cur_column
apply_discretizer <- function(data, discretizer){
  UseMethod("apply_discretizer", discretizer)
}

apply_discretizer.Discretizer <- function(data, discretizer){
  #' @exportS3Method
  .cols <- names(discretizer) %in% colnames(data)
  .cols <- names(discretizer)[.cols]
  data %>% mutate(
    across(all_of(.cols), function(i) discretizer_mutate.f(i, discretizer))
  )

}

discretizer_mutate.f <- function(i, discretizer) {
  cutter <- discretizer[[cur_column()]]
  cutter$transform(i)
}

non.discrete <- function(vec){
  non.na.vec <- vec[!is.na(vec)]
  any(round(non.na.vec) != non.na.vec)
}


discretize <- function(data, n=5, method="equalfreq"){
  non.disc <- data %>%
    select(where(is.numeric)) %>%
    select(where(non.discrete))

  discretizers <- learn_discretizers(non.disc, n=n, method=method)

  apply_discretizer(data, discretizers)
}

is.singular <- function(vec){
  length(unique(vec)) == 1
}

is.not.singular <- function(vec){
  !is.singular(vec)
}


is.categorical <- function(vec){
  is.character(vec) | is.factor(vec)
}


is.not.categorical <- function(vec){
  !is.categorical(vec)
}


paired.mutinfo <- function(data){
  #' @importFrom dplyr desc filter arrange
  #' @importFrom tidyr pivot_longer
  #' @importFrom infotheo mutinformation

  Variable <- Explainer <- `Entropy Ratio` <- NULL

  singular.cols <- data %>% select(where(is.singular)) %>% colnames()


  if (length(singular.cols)){
    msg <- paste(singular.cols, collapse=", ")
    warning(glue("Column(s) {msg} were excluded from mutual \\
                 information analysis (singular)."))
  }

  # TODO: consider discretizing all numeric data.
  # TODO: consider renaming discretized variables to identify method.

  disc  <- data %>%
    select(-all_of(singular.cols)) %>%
    mutate(across(where(is.not.categorical), as.numeric))  %>%
    discretize()

  disc[is.na(disc)] <- "NA"

  mi <- disc %>%
    mutinformation()

  entropy <- diag(mi)

  (mi / entropy) %>%
    data.frame(`Variable` = rownames(.)) %>%
    pivot_longer(!Variable, names_to  = "Explainer",
                        values_to = "Entropy Ratio") %>%
    filter(Variable != `Explainer`) %>%
    arrange(desc(`Entropy Ratio`))

}

?union
