#
#

tempfile_name <- function(filename, dir){
  .files <- list.files(dir)
  .bool <- TRUE

  while (.bool){
    suffix <- sample(c(letters, LETTERS, 0:9), 10, T) |>
      paste0(collapse="")

    .candidate <- paste0(filename, suffix)
    .bool <- any(.candidate %in% .files)
  }

  .candidate
}

report_from_template <- function(destination,
                                 name,
                                 template_path,
                                 data=NULL,
                                 browse=TRUE,
                                 ...){


  report_files <- file.path(destination,  "render_files")
  dir.create(report_files, showWarnings = F)

  to_knit <- file.path(report_files, name) %>%
    paste0(".Rmd")

  file1 <- tempfile_name(name, report_files)

  if (!is.null(data)){
    file.path(report_files, file1) %>%
      saveRDS(data, .)
  }

  report.name <- name

  template <- readLines(template_path)

  new.file <- sapply(
    template,
    function(i) glue::glue(i, .open = "%{", .close = "}%")
  )

  write(new.file, to_knit, sep = "")

  output_file <- glue::glue("../{name}")

  rendered <- rmarkdown::render(
    to_knit,
    output_file = output_file,
    clean=T
  )

  if (browse){
    browseURL(rendered)
  }

  rendered
}






