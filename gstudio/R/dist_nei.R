#' Estimation of Bray-Curtis distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the Nei's Genetic distance metric.  Assumes drift/mutation equilibrium
#'  is the main force governing your observed differences.
#' @param x A data frame with locus objects in it as well as a stratum object.
#' @param stratum A factor indicating the stratum to use.
#' @return The Nei Genetic distance
#' @note This is the bias corrected Nei's Standard genetic distance.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @importFrom reshape2 dcast 
#'  
dist_nei <- function( x, stratum="Population") {
  
  if( !is(x,"data.frame") & !is(x,"locus") )
    stop("This function requires a data.frame to work.")
  
  if( !(stratum %in% names(x) ) )
    stop( "You need to have a stratum column in the data.frame to indicate which sample is in which population.")
  
  N <- dim(x)[1]
  freqs <- frequencies( x, stratum=stratum) 
  f <- dcast( freqs, Locus + Allele ~ Stratum, value.var="Frequency", fill=0)
  strata_names <- names(f)[3:length(names(f))]
  K <- length(strata_names)
  ret <- matrix( 0, K, K )
  colnames(ret) <- rownames(ret) <- strata_names
  
  f.locus <- split( f, f$Locus )
  
  for(i in 1:K){
    p1 <- f[strata_names[i]]
    for( j in i:K) {
      if( i!=j){
        p2 <- f[strata_names[2]]
        top <- (2*N-1) * sum( p1*p2 )
        bot1 <- bot2 <- 0.
        for( k in seq_along(f.locus) ){
          p1 <- f.locus[[k]][strata_names[1]]
          p2 <- f.locus[[k]][strata_names[2]]
          bot1 <- bot1 + (2*N * sum( p1^2 ) - 1) 
          bot2 <- bot2 + (2*N * sum( p2^2 ) - 1) 
        }
        bot <- sqrt( bot1 * bot2 )
        if( bot != 0 )
          ret[j,i] <- ret[i,j] <- -1*log(top/bot) 
      }
    }
  }
  
  return(ret)
}
