#' @importFrom R6 R6Class
#' @importFrom usethis create_package
#' @importFrom pkgdown build_site
DataReport <- R6Class("DataReport", list(
  path = NA,
  report_name = NA,
  vignettes = c(),

  initialize = function(path = NULL){
    #' @importFrom usethis local_project create_package
    if (is.null(path)){
      path <- file.path(getwd(), "data.report")
    }

    self$report_name <- get_path_last(path)
    self$path <- usethis::create_package(path, open=FALSE)

    original_project <- usethis::proj_get()

    usethis::proj_set(path)

    init_pkgdown()
    dir.create(self$relative_path("report_data"), showWarnings = F)
    dir.create(self$relative_path("vignettes"), showWarnings = F)
    usethis::proj_set(original_project)


  },

  build_site = function(){
    #' @importFrom DT datatable
    local_project(self$path)
    pkgdown::build_site()
  },

  add_vignette = function(data=NULL, name=NULL, description="",
                          template_path = default_template_path(), ...){
    if (is.null(name)){
      name <- as.character(substitute(data))
    }

    add_vignette(self$path, name, description, data, ...)
    self$vignettes <- c(self$vignettes, name)
  },

  update_yaml = function(){
    options <- list(navbar = list(
      structure = list(left = c("articles"))
    ))

    config <- usethis:::pkgdown_config("docs")

    config <- append(config, options)

    config_path <- file.path(self$path, "_pkgdown.yml")

    yaml::write_yaml(config, config_path)

    config_path

  },

  use_there = function(...){
    use_there(self$path, ...)
  },

  relative_path = function(path){
    file.path(self$path, path)
  }
))

new_data_report <- function(path=NULL){
  #' Creates a data report project structure and object.
  #'
  #' @param path The path to the data report root
  #' @export
  DataReport$new(path)
}
