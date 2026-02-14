#' Plot a population graph as a single ggplot layer
#'
#' This function creates both edge and node layers for a \code{popgraph}/\code{igraph}
#'  object in a single call. Edges are drawn as segments underneath filled circle nodes.
#'  Coordinates can be mapped from vertex attributes via \code{aes(x=, y=)} or
#'  auto-computed using a force-directed layout.
#' @param graph A \code{popgraph} or \code{igraph} object (required).
#' @param mapping An \code{aes()} object. Supported aesthetics: \code{x}, \code{y}
#'  (vertex attributes for coordinates; if omitted, uses \code{layout_with_fr()}),
#'  \code{fill} (vertex attribute for node fill color),
#'  \code{color} (vertex attribute for node border color).
#' @param ... Additional arguments passed to \code{geom_point()} for the node layer
#'  (e.g., \code{alpha}, \code{stroke}).
#' @param node.size Fixed size for nodes (default 3).
#' @param edge.color Fixed color for edges (default \code{"grey60"}).
#' @param edge.width Fixed linewidth for edges (default 0.5).
#' @return A list containing a \code{geom_segment} (edges) and \code{geom_point} (nodes),
#'  which ggplot2 natively handles when added with \code{+}.
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @importFrom rlang .data as_name parse_expr
#' @importFrom stats setNames
#' @export
#' @examples
#' a <- matrix(c(0,1,0,1,1,0,0,1,0,0,0,1,1,1,1,0), nrow=4)
#' rownames(a) <- colnames(a) <- LETTERS[1:4]
#' graph <- as.popgraph(a)
#' igraph::V(graph)$x <- runif(4)
#' igraph::V(graph)$y <- runif(4)
#' igraph::V(graph)$group <- c("A","A","B","B")
#' require(ggplot2)
#' ggplot() + geom_popgraph(graph, aes(x=x, y=y))
#' ggplot() + geom_popgraph(graph, aes(x=x, y=y, fill=group))
#' ggplot() + geom_popgraph(graph)
geom_popgraph <- function(graph, mapping = aes(), ...,
                          node.size = 3,
                          edge.color = "grey60",
                          edge.width = 0.5) {

  if (!inherits(graph, "igraph")) {
    stop("The 'graph' argument must be an igraph or popgraph object.")
  }

  # Extract or compute coordinates
  if (!is.null(mapping$x) && !is.null(mapping$y)) {
    x_attr <- rlang::as_name(mapping$x)
    y_attr <- rlang::as_name(mapping$y)
    x_coords <- vertex_attr(graph, x_attr)
    y_coords <- vertex_attr(graph, y_attr)
    if (is.null(x_coords) || is.null(y_coords)) {
      stop("Vertex attributes for x/y coordinates not found in graph.")
    }
  } else {
    layout <- layout_with_fr(graph)
    x_coords <- layout[, 1]
    y_coords <- layout[, 2]
  }

  # Ensure names exist
  if (is.null(vertex_attr(graph, "name"))) {
    igraph::V(graph)$name <- paste("node", seq_along(igraph::V(graph)), sep = "-")
  }
  node_names <- igraph::V(graph)$name

  # Build edge data.frame
  el <- as_edgelist(graph, names = TRUE)
  name_idx <- setNames(seq_along(node_names), node_names)
  edge_df <- data.frame(
    x    = x_coords[name_idx[el[, 1]]],
    y    = y_coords[name_idx[el[, 1]]],
    xend = x_coords[name_idx[el[, 2]]],
    yend = y_coords[name_idx[el[, 2]]]
  )

  # Build node data.frame with all vertex attributes
  node_df <- data.frame(x = x_coords, y = y_coords, stringsAsFactors = FALSE)
  for (attr_name in vertex_attr_names(graph)) {
    node_df[[attr_name]] <- vertex_attr(graph, attr_name)
  }

  # Edge layer
  edge_layer <- ggplot2::geom_segment(
    aes(x = .data[["x"]], y = .data[["y"]], xend = .data[["xend"]], yend = .data[["yend"]]),
    data = edge_df,
    color = edge.color,
    linewidth = edge.width
  )

  # Build node aesthetic mapping
  node_aes <- aes(x = .data[["x"]], y = .data[["y"]])

  if (!is.null(mapping$fill)) {
    fill_var <- rlang::as_name(mapping$fill)
    node_aes$fill <- rlang::parse_expr(paste0(".data[['", fill_var, "']]"))
  }

  if (!is.null(mapping$colour)) {
    col_var <- rlang::as_name(mapping$colour)
    node_aes$colour <- rlang::parse_expr(paste0(".data[['", col_var, "']]"))
  } else if (!is.null(mapping$color)) {
    col_var <- rlang::as_name(mapping$color)
    node_aes$colour <- rlang::parse_expr(paste0(".data[['", col_var, "']]"))
  }

  # Node layer — shape 21 supports fill + color aesthetics
  node_layer <- ggplot2::geom_point(
    mapping = node_aes,
    data = node_df,
    size = node.size,
    shape = 21,
    ...
  )

  list(edge_layer, node_layer)
}
