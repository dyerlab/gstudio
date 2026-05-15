#' Convert popgraph nodes to a SpatialPoints object
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' \code{to_SpatialPoints()} is deprecated. The \code{sp} package is being
#' superseded by \code{sf}. Use \code{\link{to_sf}} instead, which returns an
#' \code{sf} object compatible with modern spatial workflows.
#'
#' @param x A \code{popgraph} object decorated with \code{Latitude} and
#'   \code{Longitude} vertex attributes (see \code{\link{decorate_graph}}).
#' @param stratum Ignored (retained for backwards compatibility).
#' @param longitude Name of the vertex attribute holding longitude values
#'   (default \code{"Longitude"}).
#' @param latitude Name of the vertex attribute holding latitude values
#'   (default \code{"Latitude"}).
#' @param ... Ignored.
#' @return A \code{sp::SpatialPoints} object.
#' @seealso \code{\link{to_sf}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
to_SpatialPoints <- function(x, stratum = "Name", longitude = "Longitude", latitude = "Latitude", ...) {
  .Deprecated("to_sf", msg = "to_SpatialPoints() is deprecated. Use to_sf() instead.")
  if (!inherits(x, "popgraph"))
    stop("This function requires a popgraph object to function")

  vertex.attr <- vertex_attr_names(x)
  if (!(latitude %in% vertex.attr) | !(longitude %in% vertex.attr))
    stop("Your graph should have Latitude and Longitude vertex attributes before converting to a Spatial* object.")

  coords <- cbind(x = vertex_attr(x, longitude),
                  y = vertex_attr(x, latitude))
  rownames(coords) <- igraph::V(x)$name
  sp::SpatialPoints(coords)
}
