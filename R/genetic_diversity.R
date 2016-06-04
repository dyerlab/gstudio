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
#'    \describe{
#'      \item{A}{Number of alleles}
#'      \item{Ae}{Effective number of alleles (default)}
#'      \item{A95}{Number of alleles with frequency at least five percent}
#'      \item{He}{Expected heterozygosity}
#'      \item{Ho}{Observed heterozygosity}
#'      \item{Hes}{Expected subpopulation expected heterozygosity}
#'      \item{Hos}{Expected subpopulation observed heterozygosity}
#'      \item{Fis}{Wright's Inbreeding coefficient (size corrected).}
#'      \item{Pe}{Locus polymorphic index.}
#'    }
#' @param small.N Apply small N correction
#' @param ... Other parameters
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
genetic_diversity <- function( x, stratum=NULL, mode=c("A","Ae","A95","He", "Ho", "Hos", "Hes", "Fis","Pe")[2] , small.N=FALSE, locus=NULL, ... ){
  mode <- tolower(mode)
  
  
  if( is(x,"locus"))
    x <- data.frame(Locus=x)
  
  if( !is(x,"data.frame") || length( column_class(x,"locus")) < 1 )
    stop("Either pass a data.frame or an object of type locus to this function.")
  if( !(mode %in% c("a","ae","a95","he", "ho", "hos", "hes", "fis","pe")))
    stop("Unrecognized mode passed to genetic_diversity().")
  if( missing(x) )
    stop("You must pass a data.frame to the genetic_diversity() function.")
  
  
  ## Passed with stratum 
  if( !is.null(stratum) ) {
    if( !(stratum %in% names(x)))
      stop("Requested stratum is NOT in the data you passed to genetic_diversity()...")
    
    pops <- partition( x, stratum=stratum )
    ret <- data.frame(Stratum=NA, Locus=NA, Diversity=NA )
    
    for( pop in pops){
      gd <- genetic_diversity( pops[[pop]], mode=mode, small.N=small.N, ...)
      gd$Stratum <- pop
      gd <- gd[,c(3,1,2)]
      if( names(ret)[3] == "Diversity" )
        names(ret)[3] <- names(gd)[3]
      
      ret <- rbind( ret, gd )
    }
    ret <- ret[ !is.na(ret$Stratum),]
    return( ret )
  }
  
  
  
    
  
  
  
  if( mode == "a")
    ret <- A(x, ...)
  else if( mode == "ae")
    ret <- Ae(x, ...)
  else if( mode == "a95")
    ret <- A(x,min_freq=0.05, ...)
  else if( mode == "he")
    ret <- He(x, stratum=stratum, small.N=small.N, ...)
  else if( mode == "ho")
    ret <- Ho(x, stratum=stratum, ...)
  else if( mode == "hs")
    ret <- Hs(x,stratum=stratum, small.N=small.N, ...)
  else if( mode == "fis")
    ret <- Fis(x, stratum=stratum, small.N = small.N, ...)
  else if( mode == "pe")
    ret <- Pe(x)
  else
    stop(paste("The type of diversity measure '", mode, "' you requested was not recognized.", sep=""))
    
  
  return( ret )
}
