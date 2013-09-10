#' Translate raster into ggplot object
#' 
#' This function takes a raster and translates it into
#'  a ggplot geometry layer for plotting.
#' @param raster An object of type raster
#' @param ... Passed on to \code{geom_tile} as optional arguments.
#' @return An object of type geom_tile
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
geom_surface <- function( raster, ... ) {
  if( missing(raster) )
    stop("Cannot turn raster into ggplot object without a raster to start with...")
  x <- y <- NULL
  df <- data.frame( raster::rasterToPoints( raster ) )  
  ret <- geom_raster( aes(x,y,fill=layer), data=df, hjust=0, vjust=0, ...)
  
  return( ret )
}