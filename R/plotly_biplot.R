get_comp_pair <- function(i=1, grid){
  glue::glue("PC{grid[i,]}")
}


make.annotations <- function(loadings, var1="PC1", var2="PC2"){

  n <- nrow(loadings)
  .x <- loadings[[var1]]
  .y <- loadings[[var2]]
  label <- loadings[["ID"]]

  purrr::map(
    1:n,
    function(i) list(
      x=.x[i], y=.y[i],
      showarrow = TRUE, text = "", ax=0, ay=0,
      xref = "x", yref = "y",
      axref = "x", ayref = "y"
    )
  )
}

create_buttons <- function(scores, loadings, compn_grid) {
  lapply(
    1:nrow(compn_grid),
    FUN = function(index) {
      pair <- get_comp_pair(index, compn_grid)
      x.PC <- pair[1]
      y.PC <- pair[2]

      .lis <- list(list(scores[[x.PC]]), list(scores[[y.PC]]))
      names(.lis) = c("x", "y")

      button <- list(
        method = 'update',
        args = list(
          .lis,
          list(annotations = make.annotations(loadings, var1=x.PC, var2=y.PC))
        ),
        label = glue::glue("{x.PC} - {y.PC}")
      )
    }
  )
}

missing <- (1:3)[!(1:3 %in% 1:1)]

# A <- 2
#
# plotly_biplot <- function(scores, loadings, n=3){
#   score.names <- colnames(scores)
#   pc.names <- glue("PC{1:n}")
#
#   range <- range(scores[["PC1"]])
#   # range.pc2 <- range(scores[["PC2"]])
#
#   missing <- pc.names[!pc.names %in% score.names]
#
#   if (length(missing)){
#     stop(glue("Component(s) {paste(missing, collapse='/ ')} not present \\
#               in `scores`. Check data or reduce `n`."))
#   }
#   combn_grid <- t(combn(n, 2))
#
#   plot_ly(x = ~scores[["PC1"]], y = ~scores[["PC2"]], mode = "markers",
#           name = "A", visible = T) %>%
#     layout(
#       title = "Drop down menus - Styling",
#       xaxis = list(domain = c(0.1, 1), range=range),
#       yaxis = list(title = "y", range=range),
#       updatemenus = list(
#         list(
#           y = 0.7,
#           buttons = create_buttons(scores, loadings, compn_grid)
#         )
#       ),
#       annotations = make.annotations(loadings)
#     )
# }


plotly_biplot <- function(scores, loadings, n=3){
  #' Produce an interactive biplot for unique pairs of the top `n`
  #' principal components
  #'
  #' @param scores data frame of scores for
  #'
  #'
  #' @export
  #' @importFrom plotly plot_ly layout
  #'

  score.names <- colnames(scores)
  pc.names <- glue("PC{1:n}")

  scores <- scores %>% mutate(ID = NA)
  loadings <- loadings %>% mutate(ID = rownames(.))

  {
    scores.n <- nrow(scores)
    loadings.n <- nrow(loadings)
    opacity <- c(rep(1, scores.n), rep(0, loadings.n))
    combined <- data.frame(cbind(
      rbind(scores, loadings), opacity
    ))
  }
  label <- combined[["ID"]]

  range <- range(scores[["PC1"]])
  # range.pc2 <- range(scores[["PC2"]])

  missing <- pc.names[!pc.names %in% score.names]

  if (length(missing)){
    stop(glue("Component(s) {paste(missing, collapse='/ ')} not present \\
              in `scores`. Check data or reduce `n`."))
  }
  q <- 3
  combn_grid <- t(combn(q, 2))

  plot_ly(text = label) %>%
    add_markers(x = ~combined[["PC1"]], y = ~combined[["PC2"]],

                hoverinfo = "text",
                marker = list(opacity=opacity),
                visible = T) %>%
    layout(
      title = "Drop down menus - Styling",
      xaxis = list(domain = c(0.1, 1), range=range),
      yaxis = list(title = "y", range=range),
      updatemenus = list(
        list(
          y = 0.7,
          buttons = create_buttons(combined, loadings, combn_grid)
        )
      ),
      annotations = make.annotations(loadings)
    )
}

