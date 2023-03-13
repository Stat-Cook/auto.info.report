pca_bi_prep <- function(pca){
  if (!inherits(pca, "prcomp")){
    stop("Expecting `prcomp` object")
  }

  PC.scores <- data.frame(pca$x)
  PC.loadings <- pca$rotation %>% data.frame(Label = rownames(.), Origin = 0)
  .lis <- list(
    scores = PC.scores,
    loadings = PC.loadings
  )

  class(.lis) <- "pca_bi_prep"
  .lis
}


bi.plot <- function(pca, x.index, y.index){
  #' General dispatch method for a bi-plot, intended for use
  #' on a 'prcomp' object.
  #'
  #' @param pca A `prcomp` object
  #' @param x.index numeric index of principal component to plot.
  #' @param y.index numeric index of principal component to plot.
  #'
  #' @export
  UseMethod("bi.plot", pca)
}

bi.plot.prcomp <- function(pca, x.index, y.index){
  #' @export
  pca.prep <- pca_bi_prep(pca)
  bi.plot(pca.prep, x.index, y.index)
}

max.scale <- function(vec) max(abs(vec))

bi.plot.pca_bi_prep <- function(pca.prep, x.index, y.index){
  #' @export
  #' @importFrom ggplot2 ggplot aes xlab ylab
  #' @importFrom ggplot2 geom_point geom_segment arrow unit
  #' @importFrom plotly ggplotly add_annotations
  PC.scores <- pca.prep$scores
  PC.loadings <- pca.prep$loadings

  scaler <- max.scale
  labels <- PC.loadings$Label
  origin <- PC.loadings$Origin

  pc.x <- glue("PC{x.index}")
  x.scores <- PC.scores[[pc.x]]
  x.loadings <- PC.loadings[[pc.x]]
  x.sf <- scaler(x.scores) / scaler(x.loadings)
  xlab1 <- glue("{pc.x} Scores (scatter)")

  pc.y <- glue("PC{y.index}")
  y.scores <- PC.scores[[pc.y]]
  y.loadings <- PC.loadings[[pc.y]]
  y.sf <- scaler(y.scores) / scaler(y.loadings)
  ylab1 <- glue("{pc.y} Scores (scatter)")

  sf <- min(x.sf, y.sf)

  p <- ggplot() +
    geom_point(aes(x=x.scores, y = y.scores)) +
    geom_segment(
      aes(x = origin, y = origin,
          xend = x.loadings * sf, yend = y.loadings * sf,
          text = labels),
      arrow = arrow(length = unit(0.5, "cm"))) +
    xlab(xlab1) + ylab(ylab1)
  # scale_x_continuous(sec.axis=sec_axis(~. / x.sf, name="PC1 Loadings (arrows)")) +
  # scale_y_continuous(sec.axis=sec_axis(~. / y.sf, name="PC2 Loadings (arrows)"))


  ggplotly(p, tooltip = "text")  %>%
    add_annotations( x = x.loadings * sf,
                     y = y.loadings * sf,
                     xref = "x", yref = "y",
                     axref = "x", ayref = "y",
                     text = "",
                     showarrow = T,
                     ax = origin,
                     ay = origin)
}
