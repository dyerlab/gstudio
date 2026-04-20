#' Converts a popgraph to JSON Graph Format (JGF)
#'
#' Exports a \code{popgraph} object as JGF — a structured JSON format that
#' supports graph-, node-, and edge-level metadata.  All igraph vertex and edge
#' attributes are written into the appropriate \code{metadata} blocks, making
#' the format richer than the flat \code{.pgraph} format.  Graph-level
#' properties (including optional locus names and genomic coordinates) are
#' written into a \code{metadata} object on the graph itself.
#'
#' @param graph An object of type \code{popgraph}.
#' @param file Path to write the \code{.json} file.  If omitted the JSON string
#'   is returned invisibly.
#' @param label A character string used as the graph label.  Defaults to
#'   \code{"Population Graph"}.
#' @param loci Optional locus information to embed in the graph metadata.
#'   Either a character vector of locus names, or a \code{data.frame} with a
#'   \code{name} column and, optionally, \code{chromosome} and \code{position}
#'   columns for genomic coordinates.
#' @param ... Ignored.
#' @return Invisibly returns the JSON string; writes to \code{file} when
#'   supplied.
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' \dontrun{
#' data(arapat)
#' graph <- popgraph(arapat[, c("Species", "Latitude", "Longitude")])
#'
#' # Basic export
#' to_jgf(graph, "arapat.json")
#'
#' # With locus metadata
#' loci <- data.frame(
#'   name       = c("TPI", "MP20"),
#'   chromosome = c("1", "3"),
#'   position   = c(12345, 98765)
#' )
#' to_jgf(graph, "arapat.json", label = "Arapat Network", loci = loci)
#' }
to_jgf <- function(graph, file, label = "Population Graph", loci = NULL, ...) {

  if (!is(graph, "popgraph"))
    stop("This function only works with a popgraph object.")

  # ---- nodes ----------------------------------------------------------------

  vnames <- igraph::vertex_attr(graph, "name")
  if (is.null(vnames))
    vnames <- paste0("Node-", seq_len(igraph::vcount(graph)))

  vattr_names <- setdiff(igraph::vertex_attr_names(graph), "name")

  nodes <- lapply(seq_len(igraph::vcount(graph)), function(i) {
    meta <- list()
    for (attr in vattr_names) {
      val <- igraph::vertex_attr(graph, attr)[[i]]
      if (!is.null(val) && length(val) == 1 && !is.na(val))
        meta[[attr]] <- val
    }
    if (is.null(meta[["size"]]))  meta[["size"]]  <- 10.0
    if (is.null(meta[["color"]])) meta[["color"]] <- "blue"
    list(label = vnames[i], metadata = meta)
  })
  names(nodes) <- vnames

  # ---- edges ----------------------------------------------------------------

  el        <- igraph::as_edgelist(graph, names = TRUE)
  eattr_names <- igraph::edge_attr_names(graph)

  edges <- lapply(seq_len(nrow(el)), function(i) {
    meta <- list()
    for (attr in eattr_names) {
      val <- igraph::edge_attr(graph, attr)[[i]]
      if (!is.null(val) && length(val) == 1 && !is.na(val))
        meta[[attr]] <- val
    }
    if (is.null(meta[["weight"]])) meta[["weight"]] <- 1.0
    list(source = el[i, 1], target = el[i, 2], metadata = meta)
  })

  # ---- graph-level metadata ------------------------------------------------

  graph_meta <- list()

  for (attr in igraph::graph_attr_names(graph)) {
    val <- igraph::graph_attr(graph, attr)
    if (!is.null(val))
      graph_meta[[attr]] <- val
  }

  if (!is.null(loci)) {
    if (is.character(loci)) {
      graph_meta[["loci"]] <- lapply(loci, function(l) list(name = l))
    } else if (is.data.frame(loci)) {
      if (!"name" %in% names(loci))
        stop("'loci' data.frame must have a 'name' column.")
      graph_meta[["loci"]] <- lapply(seq_len(nrow(loci)), function(i) {
        entry <- list(name = as.character(loci[["name"]][i]))
        if ("chromosome" %in% names(loci))
          entry[["chromosome"]] <- as.character(loci[["chromosome"]][i])
        if ("position" %in% names(loci))
          entry[["position"]] <- as.numeric(loci[["position"]][i])
        entry
      })
    } else {
      stop("'loci' must be a character vector or data.frame.")
    }
  }

  # ---- assemble JGF document -----------------------------------------------

  jgf_graph <- list(label = label, nodes = nodes, edges = edges)
  if (length(graph_meta) > 0)
    jgf_graph[["metadata"]] <- graph_meta

  doc <- list(graph = jgf_graph)

  json_text <- jsonlite::toJSON(doc, auto_unbox = TRUE, pretty = TRUE,
                                null = "null")

  if (!missing(file)) {
    write(json_text, file)
    invisible(json_text)
  } else {
    return(json_text)
  }
}
