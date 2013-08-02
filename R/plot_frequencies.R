#' Plot allele frequencies
#' 
#' This function allows you to plot allele frequencies for 
#'  several potential levels of stratum and loci.
#' @param x The data frame with allele frequencies from \code{frequencies}.
#' @return An object of type \code{ggplot}
#' @note This function does a pretty good job of plotting but if you have too many loci
#'  and/or strata, you may have a difficult time getting it to look right.  By default, 
#'  the function does NOTHING to prevent you from making crappy plots with too many 
#'  levels.  To plot a reduced number of loci/strata, use a subset of your data.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
plot_frequencies <- function(x){
  require(ggplot2)
  Allele <- Frequency <- NULL
  
  if( !("Frequency" %in% names(x)) | !("Allele" %in% names(x)))
    stop("Cannot plot this data.frame, it does not appear to be from frequencies().")
  
  ret <- ggplot( x ) + 
          ylim( c(0,1) ) + 
          geom_bar( aes(x=Allele,y=Frequency,fill=Allele), stat="identity" )
  
  if( "Locus" %in% names(x) ){
    
    if( "Stratum" %in% names(x) ){
      ret <- ret + facet_grid(Stratum~Locus)
    }
    else {
      ret <- ret + facet_grid(.~Locus)
    }
    
  }
  
  return( ret )
}
