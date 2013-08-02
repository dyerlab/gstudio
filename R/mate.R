#' Produces offspring
#' 
#' This function is mostly for simulation work and functions to 
#'  produce a set of offpsring for the individuals passed.
#' @param mom  This is the 'maternal' individual in that all the
#'  metadata in the \code{data.frame} that describes this individual 
#'  will be transfered to the offspring.
#' @param dad This is the 'paternal' individual and will contribute only
#'  half of its genetic compliement to the offspring.
#' @param N The number of offspring to produce.
#' @param ID The column in \code{mom} that is the identification column
#'  as all offspring from a maternal individual have the same ID (see also
#'  \code{\link{paternity}} and \code{\link{minus_mom}}).
#' @return A \code{data.frame} of offspring.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' f <- data.frame(Allele=LETTERS[1:2], Frequency=c(0.5,0.5))
#' adults <- make_population(f,N=2)
#' adults
#' mate( adults[1,], adults[2,], N=10)
mate <- function( mom, dad, N, ID="ID"){
  
  if( missing(mom) | missing(dad) )
    stop("You need to pass both parents to make an offspring using mate().")
  if(missing(N))
    stop("You need to specify how many offspring to make using mate().")
  
  locus_names <- column_class(mom,"locus")
  offnames <- setdiff( names(mom), locus_names)
  offnames <- c(offnames, "OffID")
  
  ret <- data.frame( ID=rep(mom[[ID]], N), OffID=seq(1,N) )
  
  # add the other metadata
  for( item in offnames)
    if( item != ID & item != "OffID")
      ret[[item]] <- mom[[item]]
  
  # add the loci as NA
  for( locus in locus_names) 
    ret[[locus]] <- NA
  
  
  # add the loci
  for( locus in locus_names){
    l <- rep(NA,N)
    for( i in 1:N)
      l[i] <- mom[[locus]] + dad[[locus]]
    ret[[locus]] <- locus( l, type="separated")
  }
  
  
  return( ret )
}

