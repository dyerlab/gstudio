#' Tests for Hardy-Weinberg Equilibrium
#' 
#' This function tests for Hardy-Weinberg Equilibrium using the either 
#'  the chi-square approximation or a permutation approach.
#' @param x A \code{data.frame} with one or more \code{locus} objects in it
#' @param mode The way in which the probabilities are estimated.  Possible values include
#'  'Chi': Chi-square approximation & 'Permute': A permutation approach
#' @return A \code{data.frame} with columns for Locus, Chi (the stat), df, and Prob.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples 
#'  data(arapat)
#'  sonora <- arapat[ arapat$Species=="Mainland",]
#'  hwe_test( sonora )

hwe_test <- function( x, mode=c("Chi")[1] ){
  
  if( is(x,"locus") & length(x) > 1 )
    x <- data.frame(x)
  
  if(!is(x,"data.frame")) {
    stop("This function takes a data.frame as a primary argument")
  }
    
  loci <- column_class( x, "locus")
  if( length(loci) < 1 )
    stop("You need to pass a data.frame to this function with a locus object in it...Hello?")
  
  ret <- data.frame( Locus=loci, Chi=NA, df=NA, Prob=NA )
  
  
  for( locus in loci ){
    data <- genotype_frequencies( x[[locus]] )
    alleles <- unique(as.character(alleles( x[[locus]])))
    alleles <- alleles[ !is.na(alleles) ]
    ell <- length( alleles )
    
    if( mode=="Chi") {
      if( sum(data$Observed) < 50 ) {
        warning(paste("Under 50 samples for",locus, "this may not be a good approximation."))
      }
      
      correction_factor <- 0
      if( any(data$Expected < 5 ) ) {
        warning(paste("Fewer than 5 genotypes expected at",locus,"consider collapsing alleles."))
        correction_factor <- 0.5 
      }
      chi.obs <- sum( ( abs( data$Observed - data$Expected ) - correction_factor )^2 / data$Expected )
      df <- (ell * (ell-1))/2 
      p <- 1.0 - pchisq(chi.obs,df=df)
    }
    
    ret$Chi[ ret$Locus==locus] <- chi.obs
    ret$df[ ret$Locus==locus] <- df
    ret$Prob[ ret$Locus==locus] <- p
    
  }

  ret$Prob <- format( ret$Prob, scientific=TRUE)
  return(ret)
  
  
}