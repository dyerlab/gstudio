#' Grab coordinates for strata
#' 
#' This function takes a \code{data.frame}, and a stratum and makes a data frame
#'  consisting of Stratum, Latitude, and Longitude for each stratum
#' @param x A \code{data.frame} object.
#' @param stratum The name of the stratum to partition on (default="Population").
#' @param longitude The column name of the longitude
#' @param latitude The column name of the latitude
#' @param sort.output A flag indicating if the results should be sorted alphabetically (default=FALSE)
#' @param single.stratum A flag to indicate that you only want one entry per stratum (for collapsing
#'  points within strata, Default=TRUE).
#' @return A data frame, with Stratum Latitude and Longitude, summarized by center of each stratum.
#' @importFrom sp SpatialPoints
#' @export 
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
strata_coordinates <- function( x,
                                stratum="Population", 
                                longitude="Longitude", 
                                latitude="Latitude",
                                sort.output=FALSE,
                                single.stratum=TRUE) {

  if( !inherits(x,'data.frame') ) 
    stop("You need to pass a data frame to this function.")
  
  df <- data.frame( Stratum=x[[stratum]], Longitude=x[[longitude]], Latitude=x[[latitude]] , stringsAsFactors=FALSE)
  
  ret <- df[ !duplicated(df),]
  
  
  if( single.stratum ){
    if( length( unique(ret$Stratum)) != nrow(ret)) {
      lon <- by( ret$Longitude, ret$Stratum, mean)
      lat <- by( ret$Latitude, ret$Stratum, mean)
      df <- data.frame( Stratum=names(lon), Longitude=as.numeric(lon), Latitude=as.numeric(lat))
      df <- df[ match( unique(ret$Stratum), df$Stratum), ]
      ret <- df
    }
  }

  if( sort.output )
    ret <- ret[ order(ret$Stratum),]
  
  return( ret )
}

