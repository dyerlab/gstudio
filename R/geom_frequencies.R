#' Plot allele frequencies
#' 
#' This function allows you to plot allele frequencies for 
#'  several potential levels of stratum and loci.
#' @param x The data frame with allele frequencies from \code{frequencies}.
#' @param ... Ingored but passed on to \code{facet_grid()} for optional stuff
#' @return An object of type \code{ggplot}
#' @note This function does a pretty good job of plotting but if you have too many loci
#'  and/or strata, you may have a difficult time getting it to look right.  By default, 
#'  the function does NOTHING to prevent you from making crappy plots with too many 
#'  levels.  To plot a reduced number of loci/strata, use a subset of your data.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#' data(arapat)
#' freqs <- frequencies( arapat )
#' require(ggplot2)
#' ggplot() + geom_frequencies( freqs )
#' freqs <- frequencies( arapat, loci="EN", stratum="Cluster")
#' ggplot() + geom_frequencies( freqs ) + facet_grid(.~Stratum)
geom_frequencies <- function(x, ...){

  if( !is(x,'data.frame'))
    stop("The function geom_frequencies() uses frequency data.frames from frequencies() to create plots.")
  
  Allele <- Frequency <- NULL

  
  if( !("Frequency" %in% names(x)) | !("Allele" %in% names(x)))
    stop("Cannot plot this data.frame, it does not appear to be from frequencies().")
  
  ret <- geom_bar( aes(x=Allele,y=Frequency,fill=Allele), stat="identity", data=x )
  
  return( ret )
}
