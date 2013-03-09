
#' Print a genetic statistic
#' 
#' This function prints out the observed and permuted values for the statistic
#' @param x An object of type 'genetic statistic'
#' @param ... Ignored
#' @return The text
#' @method print structure_statistic
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
print.structure_statistic <- function( x, ... ) {
  ret <- paste("Genetic Structure:", x$mode, "\n")
  ret <- paste( ret, ifelse(length(x$loci)>1, " Loci:"," locus:"), paste(x$loci,collapse=","),"\n")
  ret <- paste( ret, " Estimate:", x$estimate, "\n" )
  if( length(x$confidence ) ) {
    rng <- range( x$confidence )
    ci <- paste( rng[1], "-", rng[2])
  }
  else
    ci <- "NA"
  ret <- paste( ret, " ho confidence range: ", ci, "\n")
  if( length(x$notes))
    ret <- paste( ret, " Note:", x$notes, "\n")
  
  print.default( ret )
  
}



