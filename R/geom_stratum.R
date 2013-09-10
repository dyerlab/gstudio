#' Returns ggplot layer for population data
#' 
#' This function takes a \code{data.frame} of population data and
#'  returns a \code{geom_point()} layer for plotting.  You can also
#'  indicate shape and color attributes in the mapping (through \code{aes})
#'  that will be carried through.
#' @param mapping The aesthetic mapping, this MUST have values for 
#'  x, y, and stratum
#' @param data The \code{data.frame} from which the coordinates and other
#'  materials are to be pulled.
#' @param ... Other parameters submitted to \code{geom_point()}.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' require(ggplot2)
#' data(arapat)
#' ggplot() + geom_strata( aes(x=Longitude,y=Latitude,stratum=Population), data=arapat) + coord_equal()
#' mapping <- aes(x=Longitude,y=Latitude,stratum=Population, color=Cluster, shape=Species)
#' s <- geom_strata( mapping, data=arapat)
#' ggplot() + s + coord_equal()
#'  
geom_strata <- function( mapping=NULL, data=NULL, ...){

  if( is.null(mapping) )
    stop("You need to at least provide an aes(x,y) for this function.")

  if( is.null( mapping$x) | is.null(mapping$y) )
    stop("To plot a stratum, you must provide some x and y coordinates...")

  if( is.null(mapping$stratum) )
    stop("Usage: geom_stratum( aes(x,y,stratum), data )")
  
  if( is.null(data) )
    stop("You need to pass a data.frame with populations and coordinates in it to this function.")

  
  df <- strata_coordinates( data, 
                            stratum=as.character(mapping$stratum), 
                            longitude=as.character(mapping$x), 
                            latitude=as.character(mapping$y) )
  
  # grab colors if you are plotting them differently.
  extra_data <- data.frame( Stratum=data[[as.character(mapping$stratum)]])   
  if( "colour" %in% names(mapping))
    extra_data[[as.character(mapping$colour)]] <- data[, as.character(mapping$colour) ]
  if( "shape" %in% names(mapping))
    extra_data[[as.character(mapping$shape)]] <- data[,as.character(mapping$shape)]
  
  if( ncol(extra_data) > 1 )
    df <- merge( df, extra_data )
  
  names(df)[1] <- as.character(mapping$stratum)
  
  ret <- geom_point( mapping, data=df, ...)
  return( ret )
}