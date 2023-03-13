use_there <- function(path, ...){
  #' Allow for `usethis` package functions to be evaluated within the data
  #' report package
  #'
  #'
  #'
  #'
  original_project <- usethis::proj_get()
  usethis::proj_set(path)

  list(...)

  usethis::proj_set(original_project)
  NULL
}
