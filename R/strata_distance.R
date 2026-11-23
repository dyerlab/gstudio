#' Return distances between strata
#' 
#' Returns the matrix of physical distances among strata.  \code{x} is either
#'  the coordinate \code{data.frame} made by \code{\link{strata_coordinates}}
#'  (columns \code{Stratum}, \code{Longitude}, \code{Latitude}) or the raw data,
#'  in which case \code{strata_coordinates()} is called on it first with
#'  \code{stratum}, \code{longitude} and \code{latitude}.
#' @param x A \code{data.frame} from \code{strata_coordinates}, or a raw data
#'  \code{data.frame} with stratum and coordinate columns.
#' @param mode The type of distance to calculate.  Currently there are two 
#'  available types of physical distance, Euclidean (straight-line) and
#'  Great Circle (from the curvature of the earth; the default, which assumes
#'  decimal-degree longitude and latitude).  Use \code{"Euclidean"} for
#'  projected coordinates.
#' @param stratum,longitude,latitude Used only when \code{x} is raw data: the
#'  stratum and coordinate columns passed to \code{strata_coordinates()}
#'  (defaults \code{"Population"} or the grouping of a grouped data frame,
#'  \code{"Longitude"} and \code{"Latitude"}).
#' @return A matrix of pairwise distances among strata.
#' @note This uses the mean radius of the earth to be 6371km as the latest estimate by USGS.
#' @export 
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' data(arapat)
#' coords <- strata_coordinates(arapat)
#' D_geo <- strata_distance(coords, mode = "Circle")
#' D_geo[1:5, 1:5]
#'
#' # The same, straight from the raw data
#' D_geo <- strata_distance(arapat)
#'
#' # Individuals on a projected grid
#' data(cornus_florida)
#' adults <- cornus_florida[cornus_florida$OffID == 0, ]
#' D <- strata_distance(adults, stratum = "ID", longitude = "X", latitude = "Y",
#'                      mode = "Euclidean")
#' D[1:5, 1:5]
strata_distance <- function( x, mode=c("Euclidean","Circle")[2], stratum="Population",
                             longitude="Longitude", latitude="Latitude" ) {
  
  mode <- tolower(mode)
  
  if( !(mode %in%  c("euclidean","circle")))
    stop(paste("Unrecognized distance metric ",mode," supplied",sep=""))
  
  if( missing(x) || !inherits(x,'data.frame') ) 
    stop("You need to pass a data frame to this function.")
  
  # raw data: find the stratum coordinates first
  is_coords <- identical( names(x), c("Stratum","Longitude","Latitude") ) &&
               !length( column_class(x, "locus") )
  if( !is_coords ) {
    stratum <- .detect_stratum(x, stratum, default = "Population", explicit = !missing(stratum))
    x <- .plain_df(x)
    for( col in c(stratum, longitude, latitude) )
      if( !(col %in% names(x)) )
        stop(sprintf("cannot find column '%s'; pass a strata_coordinates() data.frame or set stratum, longitude and latitude", col))
    x <- strata_coordinates( x, stratum = stratum, longitude = longitude, latitude = latitude )
  }
  x <- .plain_df(x)
  
  K <- nrow(x)
  ret <- matrix( 0, nrow=K, ncol=K)
  colnames( ret ) <- row.names( ret ) <- x$Stratum
  
  R <- 6371
  
  for( i in 1:K){
    x1 <- as.numeric(x[i,2:3])
    for(j in i:K){
      if( i != j){
        x2 <- as.numeric(x[j,2:3])
        d <- NA
        if( mode == "euclidean")
          d <- sqrt( sum( (x1-x2)^2 )   )
        else if( mode == "circle") {
          p <- x1 * pi / 180
          q <- x2 * pi / 180
          a <- sin(0.5 * (q[2]-p[2]))
          b <- sin(0.5 * (q[1]-p[1]))
          d <- 2*R * asin( sqrt( a * a + cos( p[2]) * cos(q[2]) * b * b ))
        }
        ret[i,j] <- ret[j,i] <- d
      }
    }
  }
  
  return( ret )
}




