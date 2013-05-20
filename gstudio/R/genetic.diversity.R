#' Estimate genetic diversity among strata in a Population,
#' 
#' This function is the main one used for estimating genetic diversity among 
#'  stratua.  Given the large number of genetic diversity metrics, not all 
#'  potential types are included.  
#'  @param x A \code{data.frame} object with \code{\link{locus}} columns.
#'  @param stratum The strata by which the genetic distances are estimated.  This
#'    can be an optional parameter when estimating distance measures calculated
#'    among indiviudals (default='Population'). 
#'  @param mode The particular genetic diversity metric that you are going to use. 
#'    The \code{gstudio} package currently includes the following individual distance 
#'    measures:
#'    \itemize{
#'      \item{A}{Number of alleles}
#'      \item{Ae}{Effective number of alleles (default)}
#'      \item{A95}{Number of alleles with frequency at least five percent}
#'      \item{He}{Expected heterozygosity}
#'      \item{PIC}{Polymorphic Information Content}
#'    }
#'  @param perm_prob A flag indicating that the probability of mode==0 should be assessed using 
#'    permuation (via \code{permute_ci})
#'  @param ... Ignored in general, but can be used to pass options to \code{permute_ci}.
#'  @return A \code{data.frame} with columns for strata, diversity (mode), and potentially P(mode=0).
#'  @export
#'  @author Rodney J. Dyer <rjdyer@@vcu.edu>
genetic.diversity <- function( x, stratum=NULL, mode=c("A","Ae","A95","He","PIC")[2], perm_prob=FALSE, ...){
  if( missing(x) )
    stop("You must pass a data.frame to the genetic.diversity() function.")
  
  # make the data frame and 
  if( is.null(stratum) ) {
    ret <- data.frame( Stratum="All" )
    pops <- list(All=x)
  }
  else {
    pops <- partition(x,stratum)
    ret <- data.frame( Stratum=names(pops) )
  }
    
  
  
  
    
  
  return( ret )
}
