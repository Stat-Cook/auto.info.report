add_rule_vignette <- function(data, .rules, data_report, name=NULL, ...){
  #' Create a data quality summary report based on the supplied 'rules'.
  #' Current implementation gives:
  #'
  #'TODO: Write this
  #'
  #' @param data A data frame where the columns are the variable to be summarized.
  #' @param .rules A list of `dq_rules` to be applied
  #' @param data_report An existing `DataReport` object to add the report to
  #' @param name The name for the file/ report
  #' @param ... Other parameters to be passed to the automated report e.g. 'description'
  #'
  #' @examples
  #' \dontrun{
  #' TODO: write this
  #' }
  #'
  #' @export

  if (is.null(name)){
    name <- as.character(substitute(data))
  }

  rules.result <- apply_rules(data, .rules)

  .vignette.data <- list(
    dq_rule_report = dq_rule_report(rules.result)
  )

  data_report$add_vignette(data = .vignette.data,
                           name = name,
                           ..., template="DQ_matrix.RMD")

  data_report
}


add_summary_vignette <- function(data, data_report, name=NULL, ...){

  #' Create a variable summary report within an existing 'DataReport'.
  #' Current imlementation gives:
  #'
  #' 1. A data type dependent set of summary statistics
  #' 2. A measure of inter-variable dependce via mutual information
  #' 3. A visualization of the overall dimensionality of the data via PCA.
  #'
  #' @param data A data frame where the columns are the variable to be summarized.
  #' @param data_report An existing `DataReport` object to add the report to
  #' @param name The name for the file/ report
  #' @param ... Other parameters to be passed to the automated report e.g. 'description'
  #'
  #'
  #' @examples
  #' \dontrun{
  #' data_report <- new_data_report('~/Data.Report')
  #' add_summary_vignette(iris, data_report, "Iris Variable Summary")
  #' data_report$build_site()
  #' }
  #'
  #' @export

  if (is.null(name)){
    name <- as.character(substitute(data))
  }

  fe <- feature_extract(data)
  data_report$add_vignette(data = fe,
                  name=name,
                  ...,
                  template = "VariableSummary.Rmd")

  data_report
}

