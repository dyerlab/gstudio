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
#' @param layout How to place the nodes, as in \code{\link{plot.popgraph}}:
#'  \code{NULL} (default) uses vertex attributes \code{x}/\code{y} or
#'  \code{Longitude}/\code{Latitude} when every node has them and otherwise a
#'  Kamada-Kawai layout; or a layout name (\code{"fr"}, \code{"kk"},
#'  \code{"circle"}, \code{"mds"}), a layout function, a two-column matrix with
#'  node names as row names, or a \code{data.frame} with columns
#'  \code{name}/\code{x}/\code{y} or the output of \code{strata_coordinates()}.
#'  Named and function layouts are computed once, on the unweighted union of all
#'  graphs, with a fixed seed, so the arrangement is reproducible.
#' @param delay The time, in seconds, that each frame is shown (default 1).
#' @param width The width of the animation in pixels (default 600).
#' @param height The height of the animation in pixels (default 600).
#' @param node_size,node_labels,node_fill Node styling for the default frames,
#'  as in \code{\link{plot.popgraph}} (defaults \code{"constant"},
#'  \code{"name"}, and \code{NULL}: fill by a \code{region} vertex attribute
#'  when present, otherwise white).  Degree is taken from each frame's own
#'  graph.
#' @param loop Should the animation loop?  Either \code{TRUE} (the default)
#'  to loop forever, \code{FALSE} to play once, or a number of repetitions.
#' @param frame_plot An optional function used to draw each frame, taking
#'  \code{(layout, title)} and returning a \code{ggplot}; requires
#'  \pkg{ggraph}.  \code{layout} is a \code{ggraph} layout (from
#'  \code{ggraph::create_layout()}) holding every node at its fixed position,
#'  with node columns \code{name} and \code{present} (\code{FALSE} for nodes
#'  absent from that graph), so it can be drawn with
#'  \code{ggraph(layout) + geom_edge_link() + ...}.  The default (\code{NULL})
#'  draws each frame with the same backend as \code{plot.popgraph()}, with
#'  absent nodes faded.  Keep the coordinate limits fixed across frames if you
#'  want nodes to stay still.
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
                              layout = NULL,
                              delay = 1,
                              width = 600,
                              height = 600,
                              node_size = c("constant", "degree", "size"),
                              node_labels = c("name", "degree", "size", "none"),
                              node_fill = NULL,
                              loop = TRUE,
                              frame_plot = NULL) {

  if (!requireNamespace("gifski", quietly = TRUE))
    stop("The 'gifski' package is required. Install it with install.packages('gifski').")
  if (!is.null(frame_plot) && !is.function(frame_plot))
    stop("'frame_plot' must be NULL or a function(layout, title) returning a ggplot.")
  if (!is.null(frame_plot) && !requireNamespace("ggraph", quietly = TRUE))
    stop("A custom 'frame_plot' needs the 'ggraph' package. Install it with install.packages('ggraph').")
  if (!is.numeric(node_size)) node_size <- match.arg(node_size)
  node_labels <- match.arg(node_labels)

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
  xlim <- range(coords[, 1])
  ylim <- range(coords[, 2])
  pad_x <- max(diff(xlim) * 0.08, 1e-8)
  pad_y <- max(diff(ylim) * 0.08, 1e-8)
  limits <- list(x = xlim + c(-pad_x, pad_x), y = ylim + c(-pad_y, pad_y))

  frame_dir <- tempfile("popgraph_frames_")
  dir.create(frame_dir)
  on.exit(unlink(frame_dir, recursive = TRUE), add = TRUE)

  frames <- file.path(frame_dir, sprintf("frame_%04d.png", seq_along(graphs)))

  for (i in seq_along(graphs)) {
    p <- if (is.null(frame_plot))
      .animation_frame(graphs[[i]], coords, titles[i], node_size, node_labels, node_fill, limits)
    else
      frame_plot(.animation_frame_layout(graphs[[i]], coords), titles[i])
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
#' Resolves the \code{layout} argument of \code{animate_popgraphs()} with the
#'  shared plotting resolver into one set of coordinates for every frame.
#'  Layout names and functions are applied to the unweighted union of all
#'  edges so that no single graph's weights drive the placement.
#' @param graphs A list of \code{igraph} objects with named nodes.
#' @param all_nodes Character vector of every node name across \code{graphs}.
#' @param layout See \code{animate_popgraphs()}.
#' @return A two-column coordinate matrix with one row per node in
#'  \code{all_nodes} (row names are node names).
#' @keywords internal
#' @noRd
.animation_layout <- function(graphs, all_nodes, layout) {
  el <- do.call(rbind, lapply(graphs, as_edgelist, names = TRUE))
  union_graph <- igraph::graph_from_data_frame(
    data.frame(from = el[, 1], to = el[, 2], stringsAsFactors = FALSE),
    directed = FALSE, vertices = data.frame(name = all_nodes, stringsAsFactors = FALSE))
  union_graph <- igraph::simplify(union_graph)

  if (is.null(layout)) {
    # Vertex coordinates count only if every node carries them in some graph.
    attr_xy <- function(a, b) do.call(rbind, lapply(graphs, function(g) {
      if (!all(c(a, b) %in% igraph::vertex_attr_names(g))) return(NULL)
      data.frame(name = V(g)$name, x = igraph::vertex_attr(g, a),
                 y = igraph::vertex_attr(g, b), stringsAsFactors = FALSE)
    }))
    for (ab in list(c("x", "y"), c("Longitude", "Latitude"))) {
      d <- attr_xy(ab[1], ab[2])
      if (!is.null(d) && all(all_nodes %in% d$name)) {
        d <- d[!duplicated(d$name), ]
        m <- .coords_from_table(d, all_nodes)
        m <- matrix(as.numeric(m), ncol = 2, dimnames = list(all_nodes, c("x", "y")))
        attr(m, "source") <- if (ab[1] == "Longitude") "geographic" else "attributes"
        return(m)
      }
    }
    layout <- "kk"
  }
  .graph_layout(union_graph, layout, nodes = all_nodes)
}


#' Build the fixed-position ggraph layout for a single frame
#'
#' Adds any nodes missing from \code{graph} as isolated vertices so every frame
#'  holds the full node set, marks them with \code{present = FALSE}, and places
#'  all nodes at the shared coordinates.
#' @param graph An \code{igraph} object with named nodes.
#' @param coords The coordinate matrix (row names are nodes) from \code{.animation_layout()}.
#' @return A \code{layout_ggraph} object.
#' @keywords internal
#' @noRd
.animation_frame_layout <- function(graph, coords) {
  el <- as_edgelist(graph, names = TRUE)
  g <- igraph::graph_from_data_frame(
    data.frame(from = el[, 1], to = el[, 2], stringsAsFactors = FALSE),
    directed = FALSE,
    vertices = data.frame(name = rownames(coords),
                          present = rownames(coords) %in% V(graph)$name,
                          stringsAsFactors = FALSE))
  ggraph::create_layout(g, layout = "manual",
                        x = coords[V(g)$name, 1],
                        y = coords[V(g)$name, 2])
}


#' Build the default plot for a single animation frame
#'
#' Draws one frame with the shared \code{plot.popgraph()} backend on the fixed
#'  coordinates.  Nodes absent from the frame's graph are drawn faded so the
#'  node set is visually stable.
#' @param graph The frame's \code{igraph}.
#' @param coords The coordinate matrix from \code{.animation_layout()}.
#' @param title The frame title.
#' @param node_size,node_labels,node_fill Node styling, as in \code{plot.popgraph()}.
#' @param limits Fixed plot limits shared by all frames, \code{list(x, y)}.
#' @return A \code{ggplot} object.
#' @keywords internal
#' @noRd
.animation_frame <- function(graph, coords, title, node_size, node_labels, node_fill, limits) {
  nd <- .graph_nodes(graph, coords[, 1:2, drop = FALSE])
  present <- nd$node %in% V(graph)$name
  nd$alpha <- ifelse(present, 1, 0.2)
  node_fill <- .resolve_node_fill(graph, node_fill)
  nd <- .graph_node_fill(nd, graph, node_fill)
  ed <- .edge_segments(as_edgelist(graph, names = TRUE), coords,
                       directed = igraph::is_directed(graph))
  ed$panel <- rep("", nrow(ed))
  ed$weight <- rep(1, nrow(ed))

  .graph_canvas(nd, ed, node_size = node_size, node_labels = node_labels,
                node_fill = node_fill, arrows = igraph::is_directed(graph),
                geographic = identical(attr(coords, "source"), "geographic"),
                limits = limits) +
    ggplot2::labs(title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16),
                   plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   legend.position = "none")
}
