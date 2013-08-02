#' Grab coordinates for strata
#' 
#' This function takes a , and a stratum and makes a data frame
#'  consisting of Stratum, Latitide, and Longitude for each stratum
#' @param x A \code{data.frame} object.
#' @param stratum The name of the stratum to partition on (default="Population").
#' @param longitude The column name of the longitude
#' @param latitude The column name of the latitude
#' @return A data frame, with Stratum Latitude and Longitude, summarized by center of each stratum.
#' @export 
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
strata_coordinates <- function( 	x,
                                stratum="Population", 
                                longitude="Longitude", 
                                latitude="Latitude" ) {

  if( !inherits(x,'data.frame') ) 
    stop("You need to pass a data frame to this function.")
  
  df <- data.frame( Stratum=x[[stratum]], Longitude=x[[longitude]], Latitude=x[[latitude]] , stringsAsFactors=FALSE)
  return( df[ !duplicated(df),] )
}

