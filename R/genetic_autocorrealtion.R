#' Performs 'Autocorrelation Analysis
#' 
#' This function takes two distance matices, one for physical separation
#'  and the other for genetic separation as well as a bin size and performs
#'  the spatial autocorrelation analysis of Smouse & Peakall (1999).
#' @param P A square physical distance matrix
#' @param G A square genetic distance matrix (same size as P)
#' @param binsize The distance bins to be estimated.  These will be taken sequentially 
#'  such that the first bin will be all observations whose separation is greater than
#'  binsize[1] and less than binsize[2].
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
genetic_autocorrelation <- function( P, G, bins, mode=c("smouse","moran")[1] ){
  if( missing(P) || missing(G) || missing(binsize)){
    stop("Need P, G, and binsize for autocorrelation.")
  }
  if( dim(P) != dim(G) ){
    stop("Need both P and G to have the same dimensionality for genetic_autocorrelation()")
  }
  
  
  
}

