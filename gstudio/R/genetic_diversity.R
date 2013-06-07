#' Estimate genetic diversity among strata in a Population,
#' 
#' This function is the main one used for estimating genetic diversity among 
#'  stratua.  Given the large number of genetic diversity metrics, not all 
#'  potential types are included.  
#' @param x A \code{data.frame} object with \code{\link{locus}} columns.
#' @param stratum The strata by which the genetic distances are estimated.  This
#'    can be an optional parameter when estimating distance measures calculated
#'    among indiviudals (default='Population'). 
#' @param mode The particular genetic diversity metric that you are going to use. 
#'    The \code{gstudio} package currently includes the following individual distance 
#'    measures:
#'    \itemize{
#'      \item{A}{Number of alleles}
#'      \item{Ae}{Effective number of alleles (default)}
#'      \item{A95}{Number of alleles with frequency at least five percent}
#'      \item{He}{Expected heterozygosity}
#'    }
#' @param nperm A flag indicating that the probability of mode==0 should be assessed using 
#'    permuation (via \code{permute_ci})
#' @return A \code{data.frame} with columns for strata, diversity (mode), and potentially P(mode=0).
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#'  AA <- locus( c("A","A") )
#'  AB <- locus( c("A","B") )
#'  BB <- locus( c("B","B") )
#'  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
#'  locus2 <- c(AB,BB,AA,BB,BB,AB,AB,AA,AA,BB)
#'  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  genetic_diversity( df, mode="Ae")
genetic_diversity <- function( x, stratum=NULL, mode=c("A","Ae","A95","He")[2], nperm=0 ){
  
  if( missing(x) )
    stop("You must pass a data.frame to the genetic_diversity() function.")
  
  if( is.null(stratum))
    data <- list(x)
  else
    data <- partition(x,stratum)
  
  mode <- tolower( mode )
  
  if( mode == "a")
    ret <- A(x)
  else if( mode == "ae")
    ret <- Ae(x)
  else if( mode == "a95")
    ret <- A(x,min_freq=0.05)
  else if( mode == "He")
    ret <- He(x)
  else
    stop(paste("The type of diversity measure '", mode, "' you requested was not recognized.", sep=""))
  
  
  
    
  
  return( ret )
}
