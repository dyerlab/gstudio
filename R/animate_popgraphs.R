#' Animate a sequence of population graph topologies
#'
#' Renders each graph in a list as a frame and stitches the frames together
#'  into an animated GIF.  Node positions are computed once, from the union
#'  of all graphs, so nodes stay fixed across frames and only the edge set
#'  changes.  This makes the animation useful for comparing topologies
#'  across time periods, loci, bootstrap replicates, or alpha thresholds.
#'  Nodes that are absent from a particular graph are drawn faded in that
#'  frame.
#'
#' @param graphs A list of \code{popgraph} (or \code{igraph}) objects.  If the
#'  list is named, the names are used as frame titles.
#' @param file The path of the GIF file to write (default "popgraphs.gif").
#' @param layout How to place the nodes.  One of:
#'  \itemize{
#'    \item A character shorthand for an \code{igraph} layout: "fr" (the
#'      default, Fruchterman-Reingold), "kk" (Kamada-Kawai), "circle", or
#'      "mds".
#'    \item A layout function, such as \code{igraph::layout_with_graphopt},
#'      that takes an \code{igraph} and returns a two-column coordinate matrix.
#'    \item A two-column numeric matrix with node names as row names.
#'    \item A \code{data.frame} with columns \code{name}, \code{x}, and
#'      \code{y}, or the output of \code{strata_coordinates()} (columns
#'      \code{Stratum}, \code{Longitude}, and \code{Latitude}).
#'  }
#'  Layout functions are applied to the union of all graphs.  Force-directed
#'  layouts are stochastic, so call \code{set.seed()} first for a
#'  reproducible arrangement.
#' @param delay The time, in seconds, that each frame is shown (default 1).
#' @param width The width of the animation in pixels (default 600).
#' @param height The height of the animation in pixels (default 600).
#' @param labels A flag indicating whether node names are drawn (default TRUE).
#' @param loop Should the animation loop?  Either \code{TRUE} (the default)
#'  to loop forever, \code{FALSE} to play once, or a number of repetitions.
#' @return The path to the GIF file, invisibly.
#' @importFrom ggplot2 .data
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' \donttest{
#' if (requireNamespace("gifski", quietly = TRUE)) {
#'   data(lopho)
#'   data(upiga)
#'   graphs <- list(Lophocereus = lopho, Upiga = upiga)
#'   out <- file.path(tempdir(), "baja.gif")
#'   set.seed(42)
#'   animate_popgraphs(graphs, file = out, layout = "kk", delay = 2)
#' }
#' }
animate_popgraphs <- function(graphs,
                              file = "popgraphs.gif",
                              layout = "fr",
                              delay = 1,
                              width = 600,
                              height = 600,
                              labels = TRUE,
                              loop = TRUE) {

  if (!requireNamespace("gifski", quietly = TRUE))
    stop("The 'gifski' package is required. Install it with install.packages('gifski').")

  if (inherits(graphs, "igraph"))
    stop("Pass a list of graphs, e.g. list(graph1, graph2), not a single graph.")
  if (!is.list(graphs) || length(graphs) == 0)
    stop("'graphs' must be a non-empty list of popgraph objects.")
  if (!all(vapply(graphs, inherits, logical(1), what = "igraph")))
    stop("Every element of 'graphs' must be a popgraph or igraph object.")
  if (!is.numeric(delay) || length(delay) != 1 || is.na(delay) || delay <= 0)
    stop("'delay' must be a single positive number of seconds.")

  graphs <- lapply(graphs, function(g) {
    if (is.null(V(g)$name))
      stop("All graphs must have node names (V(graph)$name) so nodes can be matched across frames.")
    igraph::upgrade_graph(g)
  })

  titles <- names(graphs)
  if (is.null(titles))
    titles <- rep("", length(graphs))
  titles[is.na(titles) | titles == ""] <- paste("Graph", which(is.na(titles) | titles == ""))

  # Fixed node coordinates across all frames
  all_nodes <- unique(unlist(lapply(graphs, function(g) V(g)$name)))
  coords <- .animation_layout(graphs, all_nodes, layout)
  xlim <- range(coords$x)
  ylim <- range(coords$y)
  pad_x <- max(diff(xlim) * 0.08, 1e-8)
  pad_y <- max(diff(ylim) * 0.08, 1e-8)
  xlim <- xlim + c(-pad_x, pad_x)
  ylim <- ylim + c(-pad_y, pad_y)

  frame_dir <- tempfile("popgraph_frames_")
  dir.create(frame_dir)
  on.exit(unlink(frame_dir, recursive = TRUE), add = TRUE)

  frames <- file.path(frame_dir, sprintf("frame_%04d.png", seq_along(graphs)))

  for (i in seq_along(graphs)) {
    p <- .animation_frame(graphs[[i]], coords, titles[i], labels, xlim, ylim)
    grDevices::png(frames[i], width = width, height = height, res = 96)
    tryCatch(print(p), finally = grDevices::dev.off())
  }

  gifski::gifski(frames,
                 gif_file = file,
                 width = width,
                 height = height,
                 delay = delay,
                 loop = loop,
                 progress = FALSE)

  invisible(file)
}


#' Resolve a layout specification into fixed node coordinates
#'
#' Converts the \code{layout} argument of \code{animate_popgraphs()} into a
#'  single set of coordinates shared by every frame.  Layout names and
#'  functions are applied to the unweighted union of all edges so that no
#'  single graph's weights drive the placement.
#' @param graphs A list of \code{igraph} objects with named nodes.
#' @param all_nodes Character vector of every node name across \code{graphs}.
#' @param layout A layout name ("fr", "kk", "circle", "mds"), a layout
#'  function, a two-column matrix with node names as row names, or a
#'  \code{data.frame} with columns (name, x, y) or (Stratum, Longitude, Latitude).
#' @return A \code{data.frame} with columns \code{name}, \code{x}, and \code{y},
#'  one row per node in \code{all_nodes}.
#' @keywords internal
#' @noRd
.animation_layout <- function(graphs, all_nodes, layout) {

  if (is.character(layout)) {
    if (length(layout) != 1)
      stop("'layout' must be a single layout name.")
    layout <- switch(layout,
                     fr = igraph::layout_with_fr,
                     kk = igraph::layout_with_kk,
                     circle = igraph::layout_in_circle,
                     mds = igraph::layout_with_mds,
                     stop("Unknown layout '", layout, "'. Use one of 'fr', 'kk', 'circle', 'mds', ",
                          "a layout function, or a matrix/data.frame of coordinates."))
  }

  if (is.function(layout)) {
    # Unweighted union of all edges so no single graph's weights drive the layout
    el <- do.call(rbind, lapply(graphs, as_edgelist, names = TRUE))
    union_graph <- igraph::graph_from_data_frame(as.data.frame(el, stringsAsFactors = FALSE),
                                                 directed = FALSE,
                                                 vertices = data.frame(name = all_nodes))
    union_graph <- igraph::simplify(union_graph)
    xy <- layout(union_graph)
    return(data.frame(name = V(union_graph)$name,
                      x = xy[, 1],
                      y = xy[, 2],
                      stringsAsFactors = FALSE))
  }

  if (is.matrix(layout)) {
    if (ncol(layout) != 2 || is.null(rownames(layout)))
      stop("A layout matrix must have two columns and node names as row names.")
    coords <- data.frame(name = rownames(layout),
                         x = as.numeric(layout[, 1]),
                         y = as.numeric(layout[, 2]),
                         stringsAsFactors = FALSE)
  } else if (is.data.frame(layout)) {
    if (all(c("name", "x", "y") %in% names(layout))) {
      coords <- data.frame(name = as.character(layout$name),
                           x = layout$x,
                           y = layout$y,
                           stringsAsFactors = FALSE)
    } else if (all(c("Stratum", "Longitude", "Latitude") %in% names(layout))) {
      coords <- data.frame(name = as.character(layout$Stratum),
                           x = layout$Longitude,
                           y = layout$Latitude,
                           stringsAsFactors = FALSE)
    } else {
      stop("A layout data.frame needs columns (name, x, y) or (Stratum, Longitude, Latitude).")
    }
  } else {
    stop("'layout' must be a layout name, a layout function, or a matrix/data.frame of coordinates.")
  }

  missing_nodes <- setdiff(all_nodes, coords$name)
  if (length(missing_nodes))
    stop("The supplied layout has no coordinates for: ", paste(missing_nodes, collapse = ", "))

  coords[match(all_nodes, coords$name), ]
}


#' Build the plot for a single animation frame
#'
#' Draws one graph on the shared coordinates.  Nodes absent from
#'  \code{graph} are drawn faded so the node set is visually stable.
#' @param graph An \code{igraph} object with named nodes.
#' @param coords The \code{data.frame} (name, x, y) from \code{.animation_layout()}.
#' @param title The frame title.
#' @param labels A flag indicating whether node names are drawn.
#' @param xlim,ylim Fixed plot limits shared by all frames.
#' @return A \code{ggplot} object.
#' @keywords internal
#' @noRd
.animation_frame <- function(graph, coords, title, labels, xlim, ylim) {

  present <- coords$name %in% V(graph)$name
  nodes <- coords
  nodes$alpha <- ifelse(present, 1, 0.2)

  el <- as_edgelist(graph, names = TRUE)
  edges <- data.frame(x = coords$x[match(el[, 1], coords$name)],
                      y = coords$y[match(el[, 1], coords$name)],
                      xend = coords$x[match(el[, 2], coords$name)],
                      yend = coords$y[match(el[, 2], coords$name)])

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = edges,
                          ggplot2::aes(x = .data$x, y = .data$y,
                                       xend = .data$xend, yend = .data$yend),
                          colour = "grey50") +
    ggplot2::geom_point(data = nodes,
                        ggplot2::aes(x = .data$x, y = .data$y, alpha = .data$alpha),
                        size = 4, colour = "#2c7fb8") +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))

  if (labels)
    p <- p + ggplot2::geom_text(data = nodes,
                                ggplot2::aes(x = .data$x, y = .data$y,
                                             label = .data$name, alpha = .data$alpha),
                                vjust = -1.1, size = 3.5)

  p
}
