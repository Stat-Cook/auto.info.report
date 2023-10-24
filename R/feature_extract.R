style_contrasts <- function(x) {
  colnames(x) <- glue(" [{colnames(x)}]")
  x
}

to_contrast <- function(vec, style_func=style_factors){
  n <- length(vec)
  .lvls <- levels(vec)
  counts <- table(vec)[.lvls]

  weight <- 1 / counts

  contrast <- diag(length(.lvls))
  colnames(contrast) <- .lvls
  rownames(contrast) <- .lvls
  style_func(contrast)
}

contrasts.arg.func <- function(data, style_func=style_contrasts){
  data %>%
    select(where(is.factor)) %>%
    lapply(
      function(x) to_contrast(x, style_func)
    )
}

contrast_groups <- function(contrast){
  .lis <- lapply(
    names(contrast),
    function(i) {
      cols <- colnames(contrast[[i]])
      glue("{i}{cols}")
    }
  )
  names(.lis) <- names(contrast)
  .lis
}


as.character_with_na <- function(vec){
  vec <- as.character(vec)
  vec[is.na(vec)] <- "NA"
  vec
}


styled_model_matrix <- function(data, formula= ~ . , style_func=style_contrasts){
  #' Produce a model.matrix embedding with variables named as defined by `style_func`
  #'
  #' @examples
  #' styled_model_matrix(iris, Sepal.Length ~ Sepal.Width + Species)
  #'
  #' @importFrom plyr is.discrete

  data.ns <- data %>% select(is.not.singular)
  na_as_characters <- mutate(data.ns, across(where(is.discrete), as.character_with_na))
  factored_data <- mutate(na_as_characters, across(where(is.character), as.factor))

  contrasts.arg <- contrasts.arg.func(factored_data, style_func)
  con_grps <- contrast_groups(contrasts.arg)

  frm <- model.frame(~., data=factored_data, na.action = na.pass)

  mat <- stats::model.matrix(
    formula,
    data = frm,
    contrasts.arg = contrasts.arg
  ) %>%
    dplyr::as_tibble() %>%
    select(-`(Intercept)`)

  list(
    `Matrix` = mat,
    `Constrast Groups` = con_grps
  )
}


is.binary <- function(vec){
  #' Check if a vector is binary (only has values 1, 0, TRUE, or FALSE)
  #'
  #' @examples
  #'
  #' vec <- sample(c(0,1), 30, T)
  #' is.binary(vec) # True
  #'
  #' vec <- sample(c(0,1, T, F), 30, T)
  #' is.binary(vec) # True
  #'
  #' vec <- sample(4, 30, T)
  #' is.binary(vec) # False
  #'
  all(vec == 0 | vec == 1)
}

is.not.binary <- function(vec){
  !is.binary(vec)
}





group_standardize <- function(data, cols){
  #' Correct scaling for categorical variables.
  #'
  #' Standardizes so that groups of variables from the same factor have total var
  #' of one, and each variable has zero-mean.
  #'
  #' @examples
  #' mat <- as_tibble(model.matrix(~ Species, data=iris))
  #' group_standardize(mat, c("Speciesversicolor", "Speciesvirginica"))

  frm <- data %>% select(all_of(cols))

  col.means <- frm %>%
    apply(2, mean)

  total.sdev <- frm %>%
    apply(2, sd) %>% sum()

  sweep(frm, 2, col.means, FUN = "-") / total.sdev
}

scaled.data.matrix <- function(data.matrix, factors=list()){
  #' @importFrom dplyr as_tibble
  #'
  non.factor.data <- data.matrix  %>%
    select(!unlist(factors))

  non.factor.scaled <- as_tibble(scale(non.factor.data))

  .lis <- lapply(
    factors,
    function(i) group_standardize(data.matrix, i)
  )

  .lis <- prepend(.lis, list(non.factor.scaled))

  names(.lis) <- NULL

  do.call(cbind, .lis) %>% select(colnames(data.matrix))
}


scaled.data <- function(data, formula= ~ 1 + ., style_func=style_contrasts){
  result <- styled_model_matrix(data, formula, style_func)

  scaled.data.matrix(result$Matrix, result$`Constrast Groups`)
}

any_na <- function(vec){
  any(is.na(vec))
}

numeric_impute <- function(data, func=~ 0){
  #' @importFrom purrr map
  #'

  replacements <- data %>% select(where(is.numeric)) %>%
    select(where(any_na)) %>%
    purrr::map(func)

  tidyr::replace_na(data, replacements)
}

limit_cardinality_factory <- function(limit = 10){
  function(vec) {
    if (is.discrete(vec)){
      return(length(unique(vec)) <= limit)
    }

    TRUE
  }
}

col_diff_warning <- function(frm1, frm2,
                             msg = "Column(s) {.x} not present."){
  cols.frm1 <- colnames(frm1)
  cols.frm2 <- colnames(frm2)

  col.diff <- cols.frm1[!cols.frm1 %in% cols.frm2]

  if (length(col.diff)){
    .x <- paste("`", col.diff, "`", sep="", collapse = ", ")
    message(glue(msg))
  }
}


pca <- function(data, report_downsample=2500, max_cardinality=10){
  #' @importFrom stats model.matrix prcomp
  #'
  limited_cardinality <- data %>%
    select(where(limit_cardinality_factory(max_cardinality)))

  col_diff_warning(data, limited_cardinality,
                   msg = "Column(s) {.x} removed due to high cardinality")

  high_cardinality <- colnames(data)[!colnames(data) %in% colnames(limited_cardinality)]

  data.matrix <- scaled.data(data=limited_cardinality, formula= ~ .)

  imputed.data.matrix <- numeric_impute(data.matrix, ~ 0)

  pca <- prcomp(imputed.data.matrix)
  total.var <- sum(pca$sdev)

  k <- length(pca$sdev)
  bs <- rev(cumsum(1 / k:1) / k)

  pca.n <- nrow(pca$x)
  if (pca.n > report_downsample){
    pca$x <- pca$x[sample(pca.n, report_downsample),]
  }

  list(
    Scree.Summary = dplyr::tibble(Component = 1:k,
     `Variance Captured (Actual)` = pca$sdev,
     `Variance Captured (Relative)` = pca$sdev / total.var,
     `Cumulative Variance  (Actual)` = cumsum(pca$sdev),
     `Cumulative Variance (Relative)` = cumsum(pca$sdev/ total.var),
     `Broken Stick (Relative)` = bs
    ),
    PCA = pca
  )
}

feature_extract <- function(data){
  #' Define summary features for passing to data report template.
  #'
  #' @param data The data set to be summarized
  #'
  #' @examples
  #' feature_extract(iris)
  #' @export
  list(
    typed.report = typed_summary_metrics(data),
    data.report = list(nrow = nrow(data), ncol=ncol(data)),
    paired.MI = paired.mutinfo(data),
    pca = pca(data)
  )
}
