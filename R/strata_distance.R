#' Return distances between strats
#' 
#' This function takes a \code{data.frame} from the \code{strata_coordinates}
#'  function and returns the between strata distance matrix. 
#' @param x A \code{data.frame} object from \code{strata_coordinates}.
#' @param mode The type of distance to calculate.  Currently there are two 
#'  available types of physical distance, Euclidean (straight-line) and
#'  Great Circle (from the curvature of the earth)
#' @return A data frame, with Stratum Latitude and Longitude, summarized by center of each stratum.
#' @note This uses the mean radius of the earth to be 6371km as the latest estimate by USGS.
#' @export 
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
strata_distance <- function( 	x, mode=c("Euclidean","Circle")[2]) {
  
  mode <- tolower(mode)
  
  if( !(mode %in%  c("euclidean","circle")))
    stop(paste("Unrecognized distance metric ",mode," supplied",sep=""))
  
  if( missing(x) | !inherits(x,'data.frame') ) 
    stop("You need to pass a data frame to this function.")
  
  if( !is.na( column_class(x,"locus"))  )
    stop("You need to pass the coordinate data.frame from strata_coordinates() to this funciton, not the raw data.")
  
  if( !( all(names(x) == c("Stratum","Longitude","Latitude"))))
    stop("You need to pass a coordinate matrix from strata_coordiantes() to this function")
  
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




