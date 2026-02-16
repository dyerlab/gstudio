#' Returns ggplot layer for population data
#' 
#' This function takes a \code{data.frame} of population data and
#'  returns a \code{geom_point()} layer for plotting.  You can also
#'  indicate shape and color attributes in the mapping (through \code{aes})
#'  that will be carried through.
#' @param mapping The aesthetic mapping, this MUST have values for 
#'  x, y, and stratum.  
#' @param data The \code{data.frame} from which the coordinates and other
#'  materials are to be pulled.  This can be a dataframe from 
#'  \code{strata_coordinates} or one with raw genotypes.
#' @param ... Other parameters submitted to \code{geom_text_repel()}.
#' @return A \code{ggplot2} layer (\code{geom_point}).
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' library(ggplot2)
#' data(arapat)
#' ggplot() +
#'     geom_strata( aes( x=Longitude,
#'                       y=Latitude,
#'                       stratum=Population), data=arapat) +
#'    coord_equal()
#' mapping <- aes(x=Longitude,y=Latitude,stratum=Population, color=Cluster, shape=Species)
#' coords <- strata_coordinates( arapat )
geom_strata <- function( mapping=NULL, data=NULL, ...){
  x <- y <- stratum <- Longitude <- Latitude <- NULL
  
  if( is.null(data) )
    stop("You need to pass a data.frame with populations and coordinates in it to this function.")

  # specify the coordinates columns (use default if not specified)
  if( is.null( mapping$x) ) {
    if( ("Longitude" %in% names(data))) {
      x <- "Longitude"  
    } else {
      stop("You need to either specify a column for the x coordinate OR have one named Longitude")
    } 
  } else {
    x <- rlang::quo_text(quo = mapping$x)
  }
  
  if( is.null( mapping$y) ) {
    if( ("Latitude" %in% names(data))) {
      y <- "Latitude"
    } else {
      stop("You need to either specify a column for the y coordinate OR have one named Latitude")
    }
  } else {
    y <- rlang::quo_text(quo = mapping$y)
  }
    

  if( is.null(mapping$stratum) ) {
    if( ("Population" %in% names(data) ) ) {
      stratum <- "Population"
    } else {
      stop("Usage: geom_stratum( aes(x,y,stratum), data )")  
    }
  } else {
    stratum <- rlang::quo_text(quo = mapping$stratum)
  }
  
  df <- strata_coordinates( data, 
                            stratum = stratum, 
                            longitude = x, 
                            latitude = y )
  
  ret <- geom_point( aes(x=Longitude, y=Latitude), data=df) 
  return( ret )
}



