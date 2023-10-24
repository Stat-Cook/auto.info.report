set_colnames <- function(frm, .cols){
  colnames(frm) <- .cols
  frm
}

summarize_proportions <- function(frm){
  frm %>%
    summarize(across(everything(), proportion))
}


merge_with_and <- function(frm1, frm2){

  frm1_cols <- colnames(frm1)
  frm2_cols <- colnames(frm2)

  all_cols <- union(frm1_cols, frm2_cols)
  shared_cols <- intersect(frm1_cols, frm2_cols)
  unique_to_frm1 <- frm1_cols[!frm1_cols %in% frm2_cols]
  unique_to_frm2 <- frm2_cols[!frm2_cols %in% frm1_cols]

  .frm1 <- frm1 %>% select(
    all_of(unique_to_frm1)
  )

  .frm2 <- frm2 %>% select(
    all_of(unique_to_frm2)
  )

  .shared <- select(frm1, all_of(shared_cols)) &
    select(frm2, all_of(shared_cols))

  cbind(.frm1, .frm2, .shared) %>%
    select(all_of(all_cols))
}

or <- function(.x, .y) {.x | .y}

