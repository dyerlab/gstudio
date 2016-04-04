#' Create a matrix of allele frequencies for a single locus across many strata
#' 
#' This is a quick translation of locus data to a tabular stratum format.
#' @param x A \code{data.frame} with locus and strata column(s)
#' @param stratum The column name to use as the partitioning 
#' @param locus The name of the locus to use
#' @return A matrix (rows are strata, columns are alleles) of allele frequencies
#' @note This function just reshapes the \code{data.frame} from \code{frequencies}.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @importFrom reshape2 melt
#' @importFrom reshape2 dcast

frequency_matrix <- function( x, stratum="Population", locus=NULL ) {
  
  if( !(stratum %in% names(x)))
    stop("You must pass the name of the column you have your stratum names located")
  if( missing(x))
    stop("You must pass a data.frame to this function")
  if( is.null(locus))
    locus <- column_class(x,"locus")
  locus_names <- column_class(x,"locus")
#  if( !is(x,"data.frame") || (length(locus_names) < 1) )
#    stop( "You must pass the data.frame with some loci in it...  Duh.")
  if( !any(locus %in% locus_names) )
    stop( "Hello?  Thinking about giving the name of an actual locus in the data.frame" )
#  if( length(locus) > 1 ) {
#    warning("This function takes only a single locus argument, using the first one.")
#    locus <- locus[1]
#  }
  
  freqs <- frequencies( x, loci=locus, stratum=stratum )
  if( length(locus)> 1 ) 
    freqs$Allele <- paste(freqs$Locus,freqs$Allele,sep="-")
  freqs$Locus <- NULL
  m <- reshape2::melt( freqs, id.vars=c("Stratum","Allele") )
  ret <- reshape2::dcast( m, Stratum ~ Allele, value.var = "value",fill = 0)  

  return( ret )
}