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
#' @param file The path of the GIF file to write.  Defaults to a temporary
#'  file, whose path is reported with a message and returned.
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
#' @param frame_plot An optional function used to draw each frame, taking
#'  \code{(layout, title)} and returning a \code{ggplot}.  \code{layout} is a
#'  \code{ggraph} layout (from \code{ggraph::create_layout()}) holding every
#'  node at its fixed position, with node columns \code{name} and
#'  \code{present} (\code{FALSE} for nodes absent from that graph), so it can
#'  be drawn with \code{ggraph(layout) + geom_edge_link() + ...}.  The default
#'  (\code{NULL}) draws edges, nodes (absent nodes faded), and, if
#'  \code{labels = TRUE}, node names.  Keep the coordinate limits fixed across
#'  frames if you want nodes to stay still.
#' @return The path to the GIF file, invisibly.
#' @importFrom ggplot2 .data
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' \donttest{
#' if (requireNamespace("gifski", quietly = TRUE) &&
#'     requireNamespace("ggraph", quietly = TRUE)) {
#'   data(lopho)
#'   data(upiga)
#'   graphs <- list(Lophocereus = lopho, Upiga = upiga)
#'   out <- file.path(tempdir(), "baja.gif")
#'   set.seed(42)
#'   animate_popgraphs(graphs, file = out, layout = "kk", delay = 2)
#'
#'   # Style the frames yourself with ggraph
#'   if (requireNamespace("ggraph", quietly = TRUE)) {
#'     my_frame <- function(layout, title) {
#'       ggraph::ggraph(layout) +
#'         ggraph::geom_edge_link(colour = "tomato") +
#'         ggraph::geom_node_point(ggplot2::aes(alpha = present), size = 5) +
#'         ggplot2::ggtitle(title) +
#'         ggplot2::theme_void()
#'     }
#'     animate_popgraphs(graphs, file = out, frame_plot = my_frame)
#'   }
#' }
#' }
animate_popgraphs <- function(graphs,
                              file = tempfile(fileext = ".gif"),
                              layout = "fr",
                              delay = 1,
                              width = 600,
                              height = 600,
                              labels = TRUE,
                              loop = TRUE,
                              frame_plot = NULL) {

  if (!requireNamespace("gifski", quietly = TRUE))
    stop("The 'gifski' package is required. Install it with install.packages('gifski').")
  if (!requireNamespace("ggraph", quietly = TRUE))
    stop("The 'ggraph' package is required. Install it with install.packages('ggraph').")
  if (!is.null(frame_plot) && !is.function(frame_plot))
    stop("'frame_plot' must be NULL or a function(layout, title) returning a ggplot.")

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
    lay <- .animation_frame_layout(graphs[[i]], coords)
    p <- if (is.null(frame_plot))
      .animation_frame(lay, titles[i], labels, xlim, ylim)
    else
      frame_plot(lay, titles[i])
    if (!inherits(p, "ggplot"))
      stop("'frame_plot' must return a ggplot object.")
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

  if (missing(file))
    message("Animation written to ", file)
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


#' Build the fixed-position ggraph layout for a single frame
#'
#' Adds any nodes missing from \code{graph} as isolated vertices so every frame
#'  holds the full node set, marks them with \code{present = FALSE}, and places
#'  all nodes at the shared coordinates.
#' @param graph An \code{igraph} object with named nodes.
#' @param coords The \code{data.frame} (name, x, y) from \code{.animation_layout()}.
#' @return A \code{layout_ggraph} object.
#' @keywords internal
#' @noRd
.animation_frame_layout <- function(graph, coords) {
  el <- as_edgelist(graph, names = TRUE)
  g <- igraph::graph_from_data_frame(
    data.frame(from = el[, 1], to = el[, 2], stringsAsFactors = FALSE),
    directed = FALSE,
    vertices = data.frame(name = coords$name,
                          present = coords$name %in% V(graph)$name,
                          stringsAsFactors = FALSE))
  ggraph::create_layout(g, layout = "manual",
                        x = coords$x[match(V(g)$name, coords$name)],
                        y = coords$y[match(V(g)$name, coords$name)])
}


#' Build the default plot for a single animation frame
#'
#' Draws one frame with ggraph on the shared coordinates.  Nodes absent from
#'  the frame's graph are drawn faded so the node set is visually stable.
#' @param layout The \code{layout_ggraph} from \code{.animation_frame_layout()}.
#' @param title The frame title.
#' @param labels A flag indicating whether node names are drawn.
#' @param xlim,ylim Fixed plot limits shared by all frames.
#' @return A \code{ggplot} object.
#' @keywords internal
#' @noRd
.animation_frame <- function(layout, title, labels, xlim, ylim) {

  p <- ggraph::ggraph(layout) +
    ggraph::geom_edge_link(colour = "grey50") +
    ggraph::geom_node_point(ggplot2::aes(alpha = ifelse(.data$present, 1, 0.2)),
                            size = 4, colour = "#2c7fb8") +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE) +
    ggplot2::labs(title = title) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA))

  if (labels)
    p <- p + ggraph::geom_node_text(ggplot2::aes(label = .data$name,
                                                 alpha = ifelse(.data$present, 1, 0.2)),
                                    vjust = -1.1, size = 3.5)

  p
}
