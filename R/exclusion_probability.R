#' Paternity exclusion probability
#' 
#' This function estimates the multilocus
#' exclusion probability from a set of loci.
#' @param x A \code{data.frame} returned from \code{frequencies()}
#' @return A \code{data.frame} with single and potentially multilocus
#'  exclusion probabilities 
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loci <- c(locus(1:2), locus(c(1,1)), locus(c(2,2)))
#' freqs <- frequencies( loci )
#' exclusion_probability( freqs )
exclusion_probability <- function( x ) {
  
  if( "Stratum" %in% names(x) ){
    f <- factor(x$Stratum)
    strata <- levels(f)
    ret <- data.frame( Stratum=character(0), Locus=character(0), Pexcl=numeric(0), PexclMax=numeric(0) )
    
    for( i in 1:length(strata)){
      data <- x[ x$Stratum==strata[i], -1 ]
      r <- exclusion_probability( data )
      r$Stratum <- strata[i]
      ret <- rbind( ret, r )
    }
    
    ret <- ret[, c(5,1:4)]
    
  }
  
  else if( "Locus" %in% names(x ) ){
    
    f <- factor(x$Locus)
    loci <- levels(f)
    ret <- data.frame( Locus=loci, Pexcl=0, PexclMax=0 )
    
    for( i in 1:length(loci) ) {
      data <- x[ x$Locus==loci[i],-1]
      r <- exclusion_probability( data )
      ret$Pexcl[i] <- r$Pexcl[1]
      ret$PexclMax[i] <- r$PexclMax[1]
    }
    
  }
  else if( "Frequency" %in% names(x)) {
    fp <- 0
    sp <- 0
    l <- length( x$Frequency )
    for( i in 1:l){
      pi <- x$Frequency[i]
      fp <- fp + pi * (1-pi)^2
      for( j in 1:l ) {
        if( i != j ){
          pj <- x$Frequency[j]
          
          sp <- sp + pi^2*pj^2*(4 - 3*pi - 3*pj)
        }
      }
    }
    Pexcl <- (fp-0.5*sp)
    PexclMax <- 1 - (2*l^3 + l^2 - 5*l + 3) / l^4
    ret <- data.frame( Locus="-", Pexcl=Pexcl, PexclMax=PexclMax)
  }
  else
    stop("Unrecognized frequency data.frame structure passed to exclusion_probability().")
  
  ret$Fraction <- ret$Pexcl / ret$PexclMax
  
  return( ret )
  
}

