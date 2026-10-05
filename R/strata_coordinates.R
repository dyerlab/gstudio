#' Grab coordinates for strata
#' 
#' This function takes a \code{data.frame}, and a stratum and makes a data frame
#'  consisting of Stratum, Latitude, and Longitude for each stratum.
#' @param x A \code{data.frame} object.
#' @param stratum The name of the stratum to partition on (default="Population").
#' @param longitude The column name of the longitude (default="Longitude").
#' @param latitude The column name of the latitude (default="Latitude").
#' @param sort.output A flag indicating if the results should be sorted alphabetically (default=FALSE).
#' @param single.stratum A flag to indicate that you only want one entry per stratum (for collapsing
#'  points within strata, default=TRUE).
#' @return A data frame with Stratum, Longitude, and Latitude, summarized by center of each stratum.
#' @export 
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' data(arapat)
#' coords <- strata_coordinates(arapat)
#' head(coords)
strata_coordinates <- function( x,
                                stratum="Population", 
                                longitude="Longitude", 
                                latitude="Latitude",
                                sort.output=FALSE,
                                single.stratum=TRUE) {

  if( !inherits(x,'data.frame') ) 
    stop("You need to pass a data frame to this function.")
  stratum <- .detect_stratum(x, stratum, default = "Population", explicit = !missing(stratum))
  x <- .plain_df(x)
  
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

