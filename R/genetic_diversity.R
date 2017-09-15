#' Estimate genetic diversity among strata in a Population,
#' 
#' This function is the main one used for estimating genetic diversity among 
#'  strata.  Given the large number of genetic diversity metrics, not all 
#'  potential types are included.  
#' @param x A \code{data.frame} object with \code{\link{locus}} columns.
#' @param stratum The strata by which the genetic distances are estimated.  This
#'    can be an optional parameter when estimating distance measures calculated
#'    among individuals (default='Population'). 
#' @param loci The set of loci to use (default NULL will use all)
#' @param mode The particular genetic diversity metric that you are going to use. 
#'    The \code{gstudio} package currently includes the following individual distance 
#'    measures:
#'    \describe{
#'      \item{A}{Number of alleles}
#'      \item{Ae}{Effective number of alleles (default)}
#'      \item{A95}{Number of alleles with frequency at least five percent}
#'      \item{He}{Expected heterozygosity}
#'      \item{Ho}{Observed heterozygosity}
#'      \item{Hes}{Expected subpopulation heterozygosity (Nei's Corrected)}
#'      \item{Hos}{Expected subpopulation heterozygosity (Nei's Corrected)}
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
genetic_diversity <- function( x, stratum=NULL, loci=NULL, mode=c("A","Ae","A95","He", "Ho", "Hos", "Hes", "Fis","Pe")[2] , small.N=FALSE, ... ){
  rmode <- mode
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
    
    # Asking for Nei's corrected stratum estimates
    if( mode %in% c("hos","hes")) {
      if( mode == "hes")
        return( Hes(x,stratum=stratum) )
      else 
        return( Hos(x,stratum=stratum) )
    }
    
    # Not summarized across stratum
    else {
      
      if( !(stratum %in% names(x)))
        stop("Requested stratum is NOT in the data you passed to genetic_diversity()...")
      
      pops <- partition( x, stratum=stratum )
      ret <- data.frame(Stratum=NA, Locus=NA, Value=NA )
      
      for( pop in names(pops)){
        gd <- genetic_diversity( pops[[pop]], mode=mode, small.N=small.N, ...)
        gd$Stratum <- pop
        gd <- gd[,c(3,1,2)]
        if( names(ret)[3] == "Value" )
          names(ret)[3] <- names(gd)[3]
        ret <- rbind( ret, gd )
      }
      ret <- ret[ !is.na(ret$Stratum),]
      
    }
  }
  
  ## Passed without Stratum
  else { 
    if( is.null(loci) )
      loci <- column_class(x,"locus")
    if( !all( loci %in% names(x) ))
      stop("You are asking for loci not present in the data.frame passed to genetic_diversity().")
    
    ret <- data.frame( Locus=NA, Value=NA )
    
    
    for( locus in loci ){
      if( mode == "a")
        val <- A(x[[locus]], ...)
      else if( mode == "ae")
        val <- Ae(x[[locus]], ...)
      else if( mode == "a95")
        val <- A(x[[locus]],min_freq=0.05, ...)
      else if( mode == "he")
        val <- He(x[[locus]], small.N=small.N, ...)
      else if( mode == "ho")
        val <- Ho(x[[locus]], ...)
      else if( mode == "fis")
        val <- Fis(x[[locus]], stratum=stratum, small.N = small.N, ...)
      else if( mode == "pe")
        val <- Pe(x[[locus]], ...)
      else if( mode == "hes" || mode == "hos")
        stop("Hes and Hos require you to specify a stratum.")
      else
        stop(paste("The type of diversity measure '", mode, "' you requested was not recognized.", sep=""))

      ret <- rbind( ret, data.frame(Locus=locus, Value=val ) )

    }
    
    names(ret)[2] <- mode
    ret <- ret[ !is.na(ret$Locus), ]
    rownames(ret) <- seq(1,nrow(ret))
  }
    
  names(ret)[ncol(ret)] <- rmode
  return( ret )     
  
  
}
