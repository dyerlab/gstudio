#' Estimation of Bray-Curtis distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the Cavalli-Sforza & Edwards (1967)  distance metric.  Assuming 
#'  drift is the only source or differences observed among strata.
#' @param stratum The groups among which you are going to estimate genetic distances.
#' @param x The genetic data, either as a single locus or multilocus (\code{data.frame}) 
#'  object.  
#' @return A matrix of Cavalli-Sforza Genetic distance estimates.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @importFrom reshape2 dcast 
#' @examples
#'   AA <- locus( c("A","A") )
#'   AB <- locus( c("A","B") )
#'   BB <- locus( c("B","B") )
#'   AC <- locus( c("A","C") )
#'   AD <- locus( c("A","D") )
#'   BC <- locus( c("B","C") )
#'   BD <- locus( c("B","D") )
#'   CC <- locus( c("C","C") )
#'   CD <- locus( c("C","D") )
#'   DD <- locus( c("D","D") )
#'   loc1 <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD)
#'   loc2 <- c(AA,AA,AC,AA,CC,CC,AC,CC,AA,AC)
#'   df <- data.frame( Population=c(rep("Pop-A",5),rep("Pop-B",5)), TPI=loc1, PGM=loc2 )
dist_cavalli <- function( x, stratum="Population" ) {
  
  if( !is( x, "data.frame") )
    stop("You need to pass a data.frame to dist_cavalli() to work.")
  
  if( !(stratum %in% names(x)))
    stop("You need to specify the correct stratum for dist_cavalli() to work.")
  
  locus_names <- column_class( x, "locus")
  K <- length( locus_names )
  if( K==0)
    stop("You need to pass objects of type 'locus' to use for dist_cavalli().")
  if( K > 1 )
    message("Multilous estimates of Cavalli-Sforza distance are assumed to be additive.")

  freqs <- frequencies( x, stratum=stratum) 
  f <- dcast( freqs, Locus + Allele ~ Stratum, value.var="Frequency", fill=0)
  strata_names <- names(f)[3:length(names(f))]
  K <- length(strata_names)
  ret <- matrix( 0, K, K )
  colnames(ret) <- rownames(ret) <- strata_names
  
  for( i in 1:K ) {
    for( j in 1:K){
      d <- 0
      for( locus in unique( f$Locus ) ){
        px <- f[ f$Locus==locus, (i+2) ]
        py <- f[ f$Locus==locus, (j+2) ]
        d <- d + (  2/pi * sqrt( 2 * ( 1- sum( sqrt( px*py  ) ) ) ) )
      }
      ret[i,j] <- ret[j,i] <- d  
    }
  }
  
  return(ret)
}
