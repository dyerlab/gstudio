#' Convenience function for file exports
#'
#' This function is a chokepoint for exporting
#'  \code{popgraph} objects to other formats.
#' @param graph An object of type \code{popgraph}.
#' @param file The path to save the graph into.
#' @param format The type of output file to use.  Options are:
#'  \describe{
#'    \item{jgf}{Save as JSON Graph Format, supporting graph-, node-, and
#'      edge-level metadata (recommended for use with the Graphs visualizer)}
#'    \item{json}{Save as JSON format}
#'    \item{kml}{Save to KML format to view in GoogleEarth}
#'    \item{graphml}{Save as Graph Markup Language}
#'    \item{html}{Save to an interactive html format viewable in your browser}
#'    \item{pajek}{Save as input to Pajek}
#'    \item{pgraph}{Save as input for GeneticStudio (default)}
#'    \item{adjacency}{Saves as an adjacency matrix in csv format}
#'    \item{paths}{Saves as shortest paths matrix in csv format}
#'    \item{weights}{Saves as weighted adjacency matrix in csv format}
#'  }
#' @param label A graph label string used by the \code{jgf} format.
#' @param loci Optional locus metadata for the \code{jgf} format.  Either a
#'   character vector of locus names, or a \code{data.frame} with a \code{name}
#'   column and optional \code{chromosome} and \code{position} columns for
#'   genomic coordinates.
#' @param ... Passed to format-specific helpers.
#' @return Nothing
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
write_popgraph <- function(graph, file, format = "pgraph",
                            label = "Population Graph", loci = NULL, ...) {

  if (!is(graph, "popgraph"))
    stop("This requires a popgraph object")

  if (!(format %in% c("jgf","json","kml","graphml","html","pajek","pgraph",
                       "adjacency","paths","weights")))
    stop("Unrecognized output format.")

  if (missing(file))
    stop("You need to pass a file to this function.")

  switch(format,
    jgf       = to_jgf(graph, file, label = label, loci = loci, ...),
    json      = to_json(graph, file),
    html      = to_html(graph, file),
    kml       = to_kml(graph, file),
    pgraph    = to_pgraph(graph, file),
    adjacency = {
      a <- to_matrix(graph, mode = "adjacency")
      write.csv(a, file = file)
    },
    paths = {
      a <- to_matrix(graph, mode = "shortest path")
      write.csv(a, file = file)
    },
    weights = {
      a <- to_matrix(graph, mode = "edge weights")
      write.csv(a, file = file)
    },
    write.graph(graph, file, format = format)
  )
}