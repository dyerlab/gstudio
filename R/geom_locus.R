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
#' @importFrom rlang get_expr is_empty
#' @export
geom_locus <- function( mapping, data, ... ) {
  
  if( missing(data)) 
    stop("You need to pass some data use the geom_locus function.")
  
  if( missing(mapping))
    stop("You need to pass a aesthetic mapping to this function.")
  
  colname <- as.character(rlang::get_expr( mapping$x ))
  Allele <- Frequency <- Stratum <- NULL
  
  if( !(colname %in% names(data))) {
    stop("No locus in the data.frame by that name...  Come on now!  I'm not a magician.")
  }
  
  if( rlang::is_empty( mapping$fill ) ) {
    freqs <- frequencies( data, loci=colname )
    ret <- ggplot2::geom_bar( aes(x=Allele,y=Frequency), stat="identity", data=freqs ) 
  }
    
  else {
    
    fillname <- as.character( rlang::get_expr( mapping$fill ) )
    if( !(fillname %in% names(data) ) ){
      stop("No strata with the name passed.  set fill= to a real column name.")
    }
    freqs <- frequencies( data, loci=colname, stratum=fillname)
    vals <- expand.grid( Stratum=unique(freqs$Stratum), Locus=unique(freqs$Locus), Allele=unique(freqs$Allele))
    freqs <- merge( freqs, vals, all=TRUE)
    ret <- ggplot2::geom_bar( aes(x=Allele,y=Frequency, fill=Stratum), stat="identity", data=freqs, position=position_dodge(), ... )
  }
    
  
  return( ret )
  
}