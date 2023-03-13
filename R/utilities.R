globalVariables(c("."))

get_path_last <- function(path){
  #' @importFrom utils tail
  .lis <- strsplit(path, .Platform$file.sep)
  sapply(.lis, function(i) tail(i, 1))

}



write.pkgdown.yaml <- function(config_file = "_pkgdown.yml", destdir = "docs", ...){

  options <- list(...)

  config <- usethis:::pkgdown_config(destdir)
  config <- append(config, options)

  config_path <- proj_path(config_file)

  yaml::write_yaml(config, config_path)

  config_path
}


init_pkgdown <- function(config_file = "_pkgdown.yml", destdir = "docs", ...){
  #' @importFrom usethis use_build_ignore use_git_ignore write_over proj_path
  #' @importFrom yaml as.yaml
  use_build_ignore(c(config_file, destdir, "pkgdown"))
  use_git_ignore(destdir)

  # if (!file.exists(config_path)){
  #   write.pkgdown.yaml(config_file, destdir, ...)
  # }

  write.pkgdown.yaml(config_file, destdir, ...)

}


append.pkgdown.yaml <- function(config_file = "_pkgdown.yml", ...){
  options <- list(...)
  config_path <- proj_path(config_file)

  original.config <- yaml::read_yaml(config_path)
  new.config <- append(original.config, options)

  yaml::write_yaml(new.config, config_path)

  config_path
}

init_pkgdown()

append.pkgdown.yaml(A=1)

cardinality <- function(x) length(unique(x))

missing <- function(values){
  is.na(values) | is.null(values)
}

not.all.missing <- function(values){
  !all(missing(values))
}
