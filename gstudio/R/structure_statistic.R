#' General constructor for a genetic statistic
#' 
#' This is the constructor for an object of type \code{structure_statistic}.
#' @param mode Which statistic is being used.
#' @param estimate The estimated value
#' @param locus The name of the locus (or loci) used (default=NA).
#' @param ci The confidence interval (default=NA)
#' @param Hs The observed heterozygosity (default=NA)
#' @param Ht The expected heterozygosity (default=NA)
#' @param notes Any other notes that may be of interest related to the 
#'  estimation (default="").
#' @return An object of type \code{structure_statistic}
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
structure_statistic <- function( mode, estimate, locus=NA, ci=NA, Hs=NA, Ht=NA, notes=character(0) ) {
  ret <- list()
  ret$mode <- mode
  ret$estimate <- estimate
  ret$locus <- locus
  ret$confidence <- ci
  ret$Hs <- Hs
  ret$Ht <- Ht
  ret$notes <- notes
  class( ret ) <- "structure_statistic"
  return(ret)
}


