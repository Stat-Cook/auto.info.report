% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarized_type_frame.R
\name{summarize_typed_frame}
\alias{summarize_typed_frame}
\title{Produce variable summaries depending on frame data type}
\usage{
summarize_typed_frame(data, ...)
}
\description{
Produce variable summaries depending on frame data type
}
\examples{
tdf2 <- TypedDataFrame2$new(iris)
summarize_typed_frame(tdf2$parsed_data_frames[["numeric"]])

}
