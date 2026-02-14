#' 
#' 
#' This function convers the popgraph object into an object that can be plot using
#'  the leaflet library.
#' 
#' @param x A population graph
#' @param what An indication of either \code{node} or \code{edge} objects to be returned
#'   for plotting on a leaflet object.
#' @param Longitude The name of the column with the longitude (or equivalent) coordinates
#' @param Latitude The name of the column with the latitude (or equivalent) coordinate.
#' @param CRS The coordinate reference system (see https://epsg.io for values).
#' @returns A \code{data.frame} object suitable for passing to \code{leaflet}.
#' @export
#' @importFrom sf st_as_sf st_cast
#' @importFrom igraph as_data_frame
#' @importFrom dplyr group_by summarize mutate arrange
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' # library(igraph)
#' # data(lopho)
#' # data(baja)
#' # graph <- decorate_graph(lopho, baja )
#' # nodes <- to_sf( graph, what="nodes")
#' # edges <- to_sf( graph, what="edges")
#' # library( leaflet )
#' # leaflet() |>
#' #  addTiles() |>
#' #  addMarkers( data=nodes ) |>
#' #  addPolylines( data=edges )

to_sf <- function( x, what=c("nodes","edges")[1], Longitude = "Longitude", Latitude = "Latitude", CRS = 4326 ) { 
  
  # hack to get check to not complain.
  to <- from <- EdgeGroup <- weight <- NULL
  
  if( !(what %in% c("nodes","edges")) ) {
    stop("Incorrect what... what?")
  }

  nodes <- igraph::as_data_frame( x,
                                   what = "vertices")
    
  if( !(Latitude %in% names(nodes))){
    stop("You must specify a latitude coordiante.  We are trying to make a map here, right?")
  }
  if( !(Longitude %in% names(nodes))) {
    stop("You must specify a longitude coordinate.  We are trying to make a map here, right?")
  }
  


  if( what == "nodes") {

    return( sf::st_as_sf( nodes, 
                  coords=c(Longitude, Latitude),
                  crs = CRS ) )
  } else {
    
    edges <- igraph::as_data_frame( x, 
                                     what="edges")

    suppressMessages(
    rbind( merge( edges, nodes, by.x = "from", by.y = "name"),
           merge( edges, nodes, by.x = "to", by.y = "name") ) |>
      dplyr::mutate( EdgeGroup = paste( from, to, sep="-") ) |>
      dplyr::arrange( EdgeGroup ) |>
      sf::st_as_sf(coords=c(Longitude,Latitude), crs=CRS ) |>
      dplyr::group_by( EdgeGroup ) |>
      dplyr::summarize( Weight = mean(weight) ) |>
      sf::st_cast( "LINESTRING" ) -> df
    )
    
    return( df )

  }
  
}
