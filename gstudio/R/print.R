
#' Print a genetic statistic
#' 
#' This function prints out the observed and permuted values for the statistic
#' @param x An object of type 'genetic statistic'
#' @param ... Ignored
#' @return The text
#' @method print structure_statistic
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
print.structure_statistic <- function( x, ... ) {
  ret <- paste("Genetic Structure:", x$mode, "\n")
  if( !is.na(x$locus))
    ret <- paste( ret, ifelse(length(x$loci)>1, " Loci:"," locus:"), paste(x$loci,collapse=","),"\n")
  ret <- paste( ret, " Estimate:", x$estimate, "\n" )
  if( length(x$confidence ) ) {
    rng <- range( x$confidence )
    h <- hist( x$confidence, plot=F)
    df <- data.frame( Bin=h$mids, N=h$counts)
    ret <- paste( ret, " Permuted Values: ", sep="\n")
    ret <- paste( ret, df, sep="\n")
  }

  if( length(x$notes))
    ret <- paste( ret, " Note:", x$notes, "\n")
  
  cat( ret )
  
}



