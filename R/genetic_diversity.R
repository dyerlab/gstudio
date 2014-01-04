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
#'      \item{Fis}{Wright's Inbreeding coefficient (size corrected).}
#'    }
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
genetic_diversity <- function( x, stratum=NULL, mode=c("A","Ae","A95","He", "Ho", "Fis")[2] ){
  
  if( missing(x) )
    stop("You must pass a data.frame to the genetic_diversity() function.")
  
  if( !is.null(stratum)) {
    pops <- partition(x,stratum)
    ret <- data.frame(Stratum=NA,Locus=NA, Diversity=NA)
    for( pop in names(pops) ){
      gd <- genetic_diversity(pops[[pop]], mode=mode )
      gd$Stratum <- pop
      gd <- gd[, c(3,1,2)]
      names(ret)[3] <- mode
      ret <- rbind( ret, gd )
    }
    
    ret <- ret[ !is.na(ret$Stratum),]
    return( ret )
  }

    
  
  mode <- tolower( mode )
  
  if( mode == "a")
    ret <- A(x)
  else if( mode == "ae")
    ret <- Ae(x)
  else if( mode == "a95")
    ret <- A(x,min_freq=0.05)
  else if( mode == "he")
    ret <- He(x)
  else if( mode == "ho")
    ret <- Ho(x)
  else if( mode == "fis")
    ret <- Fis(x)
  else
    stop(paste("The type of diversity measure '", mode, "' you requested was not recognized.", sep=""))
  
  
  
    
  
  return( ret )
}
