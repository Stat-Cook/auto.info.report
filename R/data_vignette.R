default_template_path <- function(){
  system.file("templates", "Template.RMD", package="MI.report")

}

DataVignette <- R6Class("DataVignette", list(
  project_path = NA,
  name = NA,
  description = NA,
  data.folder = "report_data",
  template = NA,

  initialize = function(project_path, template_path=default_template_path()){
    self$project_path = project_path
    self$template = readLines(template_path)
  },

  vignette.path = function(name) {
    #' @importFrom glue glue
    file <- glue("{name}.Rmd")
    file.path(self$project_path, "vignettes", file)
  },

  data.path = function(name) {
    file.path(self$project_path, self$data.folder, name)
  },

  save.data = function(data, name) saveRDS(data, self$data.path(name)),

  to.vignette = function(name, data = NULL, description="", ...){

    if (!is.null(data)){
      self$save.data(data, name)
    }
    file1 <- self$data.path(name)

    {
      report.name <- name

      new.file <- sapply(
        self$template,
        function(i) glue(i, .open="%{", .close="}%")
      )
    }
    write(new.file, self$vignette.path(name), sep = "")
  }
))

add_vignette <- function(project_path, name, description="", data=NULL,
                         template_path = default_template_path(), ...){
  data_vignette <- DataVignette$new(project_path, template_path)

  if (!is.null(data)){
    data_vignette$to.vignette(name, data, description, ...)
  }

  data_vignette
}
