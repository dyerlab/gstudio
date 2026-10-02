#' Create a matrix of allele frequencies for a single locus across many strata
#' 
#' This is a quick translation of locus data to a tabular stratum format.
#' @param x A \code{data.frame} with locus and strata column(s)
#' @param stratum The column name to use as the partitioning 
#' @param loci The name of the locus to use
#' @return A matrix (rows are strata, columns are alleles) of allele frequencies
#' @note This function just reshapes the \code{data.frame} from \code{frequencies}.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#' data(arapat)
#' fm <- frequency_matrix(arapat, stratum = "Population", loci = "LTRS")
#' head(fm)

frequency_matrix <- function( x, stratum="Population", loci=NULL ) {
  
  if( !(stratum %in% names(x)))
    stop("You must pass the name of the column you have your stratum names located")
  if( missing(x))
    stop("You must pass a data.frame to this function")
  if( is.null(loci))
    loci <- column_class(x,"locus")
  locus_names <- column_class(x,"locus")
  if( !any(loci %in% locus_names) )
    stop( "Hello?  Thinking about giving the name of an actual locus in the data.frame" )

  
  freqs <- frequencies( x, loci=loci, stratum=stratum )
  if( length(loci)> 1 ) 
    freqs$Allele <- paste(freqs$Locus,freqs$Allele,sep="-")
  freqs$Locus <- NULL
  mat <- as.matrix(stats::xtabs(Frequency ~ Stratum + Allele, data = freqs))
  ret <- data.frame(Stratum = rownames(mat), unclass(mat), check.names = FALSE, stringsAsFactors = FALSE)
  rownames(ret) <- NULL
  
  return( ret )
}