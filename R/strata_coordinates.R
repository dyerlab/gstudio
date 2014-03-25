#' Grab coordinates for strata
#' 
#' This function takes a , and a stratum and makes a data frame
#'  consisting of Stratum, Latitide, and Longitude for each stratum
#' @param x A \code{data.frame} object.
#' @param stratum The name of the stratum to partition on (default="Population").
#' @param longitude The column name of the longitude
#' @param latitude The column name of the latitude
#' @param as.SpatialPoints A flag indicating what kind of coordinates to return 
#'  should be turned into a SpatialPoints object (TRUE) or as a \code{data.frame} (FALSE,
#'  the default)
#' @param sort.output A flag indicating if the results should be sorted alphabetically (default=FALSE)
#' @return A data frame, with Stratum Latitude and Longitude, summarized by center of each stratum.
#' @importFrom sp SpatialPoints
#' @export 
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
strata_coordinates <- function( x,
                                stratum="Population", 
                                longitude="Longitude", 
                                latitude="Latitude",
                                as.SpatialPoints=FALSE,
                                sort.output=FALSE) {

  if( !inherits(x,'data.frame') ) 
    stop("You need to pass a data frame to this function.")
  
  df <- data.frame( Stratum=x[[stratum]], Longitude=x[[longitude]], Latitude=x[[latitude]] , stringsAsFactors=FALSE)
  
  ret <- df[ !duplicated(df),]
  
  if( sort.output )
    ret <- ret[ order(ret$Stratum),]
  
  
  if( as.SpatialPoints ) {
    coords <- cbind( x=ret$Longitude,
                     y=ret$Latitude) 
    rownames( coords ) <- ret$Stratum
    ret <- sp::SpatialPoints(coords)
  }
  
  return( ret )
}

