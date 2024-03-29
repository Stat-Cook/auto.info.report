% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_implemented_vigenttes.R
\name{add_summary_vignette}
\alias{add_summary_vignette}
\title{Create a variable summary report within an existing 'DataReport'.
Current imlementation gives:}
\usage{
add_summary_vignette(data, data_report, name = NULL, ...)
}
\arguments{
\item{data}{A data frame where the columns are the variable to be summarized.}

\item{data_report}{An existing `DataReport` object to add the report to}

\item{name}{The name for the file/ report}

\item{...}{Other parameters to be passed to the automated report e.g. 'description'}
}
\description{
1. A data type dependent set of summary statistics
2. A measure of inter-variable dependce via mutual information
3. A visualization of the overall dimensionality of the data via PCA.
}
\examples{
\dontrun{
data_report <- new_data_report('~/Data.Report')
add_summary_vignette(iris, data_report, "Iris Variable Summary")
data_report$build_site()
}

}
