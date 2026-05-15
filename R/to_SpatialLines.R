#' Convert popgraph edges to a SpatialLines object
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' \code{to_SpatialLines()} is deprecated. The \code{sp} package is being
#' superseded by \code{sf}. Use \code{\link{to_sf}} instead, which returns an
#' \code{sf} object compatible with modern spatial workflows.
#'
#' @param graph A \code{popgraph} object decorated with \code{Latitude} and
#'   \code{Longitude} vertex attributes (see \code{\link{decorate_graph}}).
#' @param latitude Name of the vertex attribute holding latitude values
#'   (default \code{"Latitude"}).
#' @param longitude Name of the vertex attribute holding longitude values
#'   (default \code{"Longitude"}).
#' @param ... Ignored.
#' @return A \code{sp::SpatialLines} object.
#' @seealso \code{\link{to_sf}}
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
to_SpatialLines <- function(graph, latitude = "Latitude", longitude = "Longitude", ...) {
  .Deprecated("to_sf", msg = "to_SpatialLines() is deprecated. Use to_sf() instead.")
  if (!inherits(graph, "popgraph"))
    stop("This function requires a popgraph object to function")

  node.names <- igraph::V(graph)$name
  vertex.attr <- vertex_attr_names(graph)
  if (!(latitude %in% vertex.attr) | !(longitude %in% vertex.attr))
    stop("Your graph should have Latitude and Longitude vertex attributes before converting to a Spatial* object.")

  lat <- vertex_attr(graph, latitude)
  lon <- vertex_attr(graph, longitude)

  all.edges <- as_edgelist(graph)
  edgeList  <- vector("list", nrow(all.edges))

  for (i in seq_len(nrow(all.edges))) {
    idx1 <- which(node.names == all.edges[i, 1])
    idx2 <- which(node.names == all.edges[i, 2])
    coord <- matrix(c(lon[idx1], lat[idx1], lon[idx2], lat[idx2]),
                    nrow = 2, byrow = TRUE,
                    dimnames = list(c(node.names[idx1], node.names[idx2]), c("x", "y")))
    edgeName      <- paste("Edge", node.names[idx1], node.names[idx2])
    edgeList[[i]] <- sp::Lines(list(sp::Line(coord)), ID = edgeName)
  }
  sp::SpatialLines(edgeList)
}
