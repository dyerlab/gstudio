#' Estimates resistance rasters from binary feature raster.
#' 
#' This function produces a 
#' 
#' @param raster A base \code{raster} object from which to base others.
#' @param feature_name The prefix for output files for this raster feature (default='layer').
#' @param costs A vector of cost rasters to iterate across (default=c(2, 5, 10, 50, 100)).  
#' @param symmetric A flag indicating that costs out of as well as into features will be estimated.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples 
#' vals <- sample( c(0,1), size=49, replace=TRUE)
#' m <- matrix( vals, nrow=7)
#' r <- raster::raster( m )
#' create_resistances( r )
#' 
create_resistances <- function( x, feature_name="layer", costs=c(2,5,10,50,100), symmetric=TRUE){
  if( !is(x,"RasterLayer"))
    stop("This function requires a base (binary) raster to work.")
  if( !any( values(x)==0))
    stop("You need to give this funciton a binary raster, stupid!")
  r <- x
  r[ r > 0 ] <- 1
   for( cost in costs) {
    layer <- (cost-1)*r + 1
    ofile <- paste(feature_name, cost,"1","rda",sep=".")
    save(layer, file = ofile)
    if( symmetric ){
      layer <- (cost-1)*( abs(r-1)) + 1
      ofile <- paste(feature_name, "1",cost,"rda",sep=".")
      save(layer, file=ofile)
    }
  }
}


