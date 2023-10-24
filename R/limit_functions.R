lower_limit <- function(frm, limit=0){
  summarize(frm, across(everything(), function(i) all(i >= limit, na.rm=T)))
}

upper_limit <- function(frm, limit=Inf){
  summarize(frm, across(everything(), function(i) all(i <= limit, na.rm=T)))
}

pattern_match <- function(vec, patterns){
  lapply(
    patterns,
    function(pattern) str_detect(vec, pattern)
  ) |>
    do.call(what=cbind) |>
    apply(1, any) |>
    all()
}

lcs.f <- function(s1, s2){
  l1 <- str_length(s1)
  l2 <- str_length(s2)
  min.adist <- abs(l1 - l2)

  max.corrected.adist <- min(l1, l2)

  1 - (adist(s1, s2) - min.adist) / max.corrected.adist
}

lcs <- Vectorize(lcs.f, c("s1", "s2"))

str_delete_all <- function(string, pattern){
  stringr::str_replace_all(string, pattern, "")
}

preprocess_strings.f <- function(val, patterns_to_remove){
  patterns_to_remove <- tolower(patterns_to_remove)
  .vec <- append(val, patterns_to_remove, after=1)
  reduce(.vec,  str_delete_all) |>
    trimws()
}

preprocess_strings <- Vectorize(preprocess_strings.f, c("val"))

intra_var_preproc <- function(vec){
  strings <- unique(vec)

  as.character(strings) |>
    combn(2, simplify = F) |>
    do.call(what = rbind)
}

inter_var_preproc <- function(vec1, vec2){
  s1 <- as.character(unique(vec1))
  s2 <- as.character(unique(vec2))

  expand.grid(X1 = s1, X2 = s2)
}

variable_lcs <- function(vec1, vec2=NULL, patterns_to_remove = c()){

  if (is.null(vec2)){
    frm <- intra_var_preproc(vec1)
  } else {
    frm <- inter_var_preproc(vec1, vec2)
  }

  data.frame(frm) |>
  mutate(
    X1preproc = preprocess_strings(tolower(X1), patterns_to_remove),
    X2preproc = preprocess_strings(tolower(X2), patterns_to_remove)
  ) |>
  mutate(
    Similarity = lcs(X1preproc, X2preproc)
  ) |>
  arrange(desc(Similarity))
}

variable_lcs_limit <- function(vec, limit=0.6){
  result <- variable_lcs(vec)

  any(result$Similarity > limit)
}
