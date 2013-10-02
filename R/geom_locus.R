#' Translate a vector of \code{locus} objects into a \code{gemo_bars} layer
#' 
#' This function takes a data frame containing genetic data and returns a 
#'  \code{geom_bars} layer for \code{ggplot} integration.
#' @param mapping The aesthetic mapping (e.g., which locus to use).  Use 
#'  \code{aes(x=LOCUS_NAME)} to specify which locus is being used.
#' @param data A \code{data.frame} containing one or more loci to be plot
#' @param ... Added to geom_bar 
#' @return A formatted set of \code{ggplot} objects to be plot
#' @note If using more than one stratum, use fill=STRATA_NAME for partitioning
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#' require(ggplot2)
#' loci <- c( locus(1:2), locus(2:3), locus(c(1,1)), locus(1:2) )
#' data <- data.frame( Population=c("A","A","B","B"), Locus=loci)
#' ggplot() + geom_locus( aes(x=Locus, fill=Population), data=data )
geom_locus <- function( mapping, data, ... ) {
  
  if( missing(data)) 
    stop("You need to pass some data use the geom_locus function.")
  
  if( missing(mapping))
    stop("You need to pass a aesthetic mapping to this function.")
  
  if( is.null( mapping$x ))
    stop("Indicate which locus to use by setting aes(x=LOCUS).")
  
  if( !(as.character(mapping$x) %in% names(data)))
    stop("The requested locus is not in the data.frame...")
  
  if( !is.null(mapping$fill) ) {
    if( !(as.character(mapping$fill) %in% names(data)))
      stop("The requested stratum is not in the data.frame...")
  } 
    
  Allele <- Frequency <- Stratum <- NULL
  
  if( is.null(mapping$fill) ) {
    freqs <- frequencies( data, loci=as.character(mapping$x) )
    ret <- geom_bar( aes(x=Allele,y=Frequency), stat="identity", data=freqs, binwidth=1 ) 
  }
    
  else {
    freqs <- frequencies( data, loci=as.character(mapping$x), stratum=as.character(mapping$fill))
    vals <- expand.grid( Stratum=unique(freqs$Stratum), Locus=unique(freqs$Locus), Allele=unique(freqs$Allele))
    freqs <- merge( freqs, vals, all=TRUE)
    ret <- geom_bar( aes(x=Allele,y=Frequency, fill=Stratum), stat="identity", data=freqs, position=position_dodge(), ... )
  }
    
  
  return( ret )
  
}