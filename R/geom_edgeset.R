#' Plotting of a population graph edge set using ggplot neumonic
#'
#' This function allows you to layer the edgeset of a \code{popgraph}
#'  object
#' @param mapping The aesthetic mapping as an \code{aes()} object.  This aesthetic
#'  must at least have values for x and y
#' @param graph The popgraph/igraph object to be plot
#' @param directed A flag indicating that you should only plot the edge
#'  with the largest weight if more than one edge connects nodes.
#' @param ... Largely ignored.
#' @return A formatted geom_segment object for addition to a ggplot()
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @import sampling
#' @importFrom rlang .data
#' @export
#' @examples
#' a <- matrix( c(0,1,0,1,1,0,0,1,0,0,0,1,1,1,1,0),nrow=4)
#' rownames(a) <- colnames(a) <- LETTERS[1:4]
#' graph <- as.popgraph(a)
#' igraph::V(graph)$x <- runif(4)
#' igraph::V(graph)$y <- runif(4)
#' igraph::E(graph)$weight <- 1 + rpois(4,2)
#' require(ggplot2)
#' ggplot() + geom_edgeset( aes(x=x,y=y), graph )
#' ggplot() + geom_edgeset( aes(x=x,y=y), graph, color="darkblue" )
#' ggplot() + geom_edgeset( aes(x=x,y=y, linewidth=weight), graph, color="darkblue" )
#' ggplot() + geom_edgeset( aes(x=x,y=y, color=weight), graph, color="darkblue" )
#' require(grid)
#' ggplot() + geom_edgeset( aes(x=x,y=y), graph, directed=TRUE, arrow=arrow(length=unit(0.5,"cm")) )
geom_edgeset <- function(mapping = NULL, graph = NULL, directed = FALSE, ...) {
  X <- Y <- NULL

  # catch errors with missing
  if (is.null(mapping)) {
    stop("You need at least aes(x,y) for aesthetic mapping in this function.")
  }
  if (is.null(graph)) {
    stop("You cannot plot a graph without a graph...")
  }

  # take care of directed.
  if (directed) {
    d <- as_adjacency_matrix(graph, type = "both", attr = "weight", sparse = FALSE)
    K <- length(igraph::V(graph))
    for (i in 1:K) {
      for (j in i:K) {
        if (d[i, j] > 0 & d[j, i] > 0) {
          if (d[i, j] > d[j, i]) {
            d[j, i] <- 0
          } else {
            d[i, j] <- 0
          }
        }
      }
    }
    g <- graph_from_adjacency_matrix(d, mode = "directed", weighted = TRUE)
    df <- to_data.frame(graph)
    graph <- decorate_graph(g, df, stratum = "name")
  }

  # grab mapping labels not in the vertex attributes
  edge.attr <- c(edge_attr_names(graph), vertex_attr_names(graph))
  mappingNames <- names(mapping)[names(mapping) != "label"]
  for (name in mappingNames) {
    key <- as.character(mapping[[name]])[2]
    if (!(key %in% edge.attr)) {
      stop(paste(
        "Aesthetic mapping variable ",
        key,
        " was not found in the edge attributes of this graph",
        sep = ""
      ))
    }
  }
  if (is.null(mapping$x) | is.null(mapping$y)) {
    stop(
      "To plot a graph, you need coordinates and they must be attributes of the vertices in the graph."
    )
  }

  X1 <- X2 <- Y1 <- Y2 <- size <- linewidth <- x <- y <- color <- colour <- NULL

  x <- vertex_attr(graph, as.character(mapping$x)[2])
  y <- vertex_attr(graph, as.character(mapping$y)[2])
  if (is.null(vertex_attr(graph, "name"))) {
    igraph::V(graph)$name <- paste(
      "node",
      1:length(igraph::V(graph)),
      sep = "-"
    )
  }

  # find the coordinates to all the segments and make into a data.frame
  layout <- matrix(cbind(x, y), ncol = 2)
  colnames(layout) <- c("X1", "X2")
  rownames(layout) <- igraph::V(graph)$name
  coords <- data.frame(
    name = igraph::V(graph)$name,
    X1 = layout[, 1],
    X2 = layout[, 2]
  )
  edgelist <- as_edgelist(graph)
  df <- data.frame(coords[edgelist[, 1], 2:3], coords[edgelist[, 2], 2:3])
  colnames(df) <- c("X1", "Y1", "X2", "Y2")

  has_lw <- !is.null(mapping$linewidth) || !is.null(mapping$size)
  has_col <- !is.null(mapping$color) || !is.null(mapping$colour)

  lw_attr <- if (!is.null(mapping$linewidth)) {
    as.character(mapping$linewidth)[2]
  } else if (!is.null(mapping$size)) {
    as.character(mapping$size)[2]
  } else {
    NULL
  }

  if (has_lw & has_col) {
    df$linewidth <- edge_attr(graph, lw_attr)
    df$color <- edge_attr(graph, as.character(mapping$colour)[2])
    ret <- ggplot2::geom_segment(
      aes(x = X1, y = Y1, xend = X2, yend = Y2, linewidth = linewidth, color = color),
      data = df,
      ...
    )
  } else if (has_lw) {
    df$linewidth <- edge_attr(graph, lw_attr)
    ret <- ggplot2::geom_segment(
      aes(x = X1, y = Y1, xend = X2, yend = Y2, linewidth = linewidth),
      data = df,
      ...
    )
  } else if (has_col) {
    lbl <- as.character(mapping$colour)[2]
    df[[lbl]] <- edge_attr(graph, lbl)
    df <- df[order(df[[lbl]]), ]
    ret <- ggplot2::geom_segment(
      aes(x = X1, y = Y1, xend = X2, yend = Y2, color = .data[[lbl]]),
      data = df,
      ...
    )
  } else {
    ret <- ggplot2::geom_segment(
      aes(x = X1, y = Y1, xend = X2, yend = Y2),
      data = df,
      ...
    )
  }

  return(ret)
}
