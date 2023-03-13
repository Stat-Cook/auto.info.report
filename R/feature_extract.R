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

data <- data.frame(A = rnorm(100), B = sample(letters[1:4], 100, T))
style_func <- style_contrasts
formula <-  ~ .

styled_model_matrix <- function(data, formula= ~ . , style_func=style_contrasts){
  factored_data <- mutate(data, across(where(is.character), as.factor))

  contrasts.arg <- contrasts.arg.func(factored_data, style_func)
  con_grps <- contrast_groups(contrasts.arg)

  mat <- stats::model.matrix(
    formula,
    data = factored_data,
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
  frm <- data %>% select(all_of(cols))

  col.means <- frm %>%
    apply(2, mean)

  total.sdev <- frm %>%
    apply(2, sd) %>% sum()

  sweep(frm, 2, col.means, FUN = "-") / total.sdev
}

scaled.data.matrix <- function(data.matrix, factors=list()){
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


pca <- function(data, report_downsample=2500){
  #' @importFrom stats model.matrix prcomp
  data.matrix <- scaled.data(data=data, formula= ~ .)

  pca <- prcomp(data.matrix)
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
  #' @export
  list(
    typed.report = typed_summary_metrics(data),
    data.report = list(nrow = nrow(data), ncol=ncol(data)),
    paired.MI = paired.mutinfo(data),
    pca = pca(data)
  )
}
