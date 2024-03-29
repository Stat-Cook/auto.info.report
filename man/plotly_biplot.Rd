% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotly_biplot.R
\name{plotly_biplot}
\alias{plotly_biplot}
\title{Produce an interactive biplot for unique pairs of the top `n`
principal components}
\usage{
plotly_biplot(scores, loadings, n = 3)
}
\arguments{
\item{scores}{data frame of scores for}
}
\description{
Produce an interactive biplot for unique pairs of the top `n`
principal components
}
\examples{
\dontrun{
mat <- model.matrix(~., data=iris)
pca <- prcomp(mat)

scaled_loadings <- data.frame(sweep(pca$rotation, 2, 2*pca$sdev, FUN = "*"))
scores <- data.frame(pca$x)

plotly_biplot(scores, scaled_loadings)
}
}
