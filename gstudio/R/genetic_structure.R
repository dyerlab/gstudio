#' Estimation of , genetic structure statistics.
#' 
#' This function estimates common , genetic statistics.
#' @param x An object of type \code{data.frame} with at least a single column
#'  of type \code{\link{locus}}
#' @param stratum The stratum to use as groupings (default='Population').
#' @param mode Whic statistic to estimate.  Current options include:
#' \itemize{
#'  \item{Gst}{Nei's Gst (not Berg and Hamrick)}
#'  \item{Gst_prime}{Hedrick's correction of Nei's Gst for diverse loci}
#'  \item{Dest}{Joost's estimate}
#'  \item{AMOVA}{Analysis of MOlecular VAriance}
#' }
#' @param loci The name of loci to be used.
#' @param nperm The number of permutations used to test the hypothesis that
#'  the parameter = 0.
#' @param verbose Print out progress (default=TRUE)
#' @return An object of type \code{genetic_structure}.
#' @note The multilocus estimation of these parameters is estimated following the
#'  suggestions of Culley et al. (2001) A comparison of two methods of calculating Gst, 
#'  a genetic measure of , differentiation.  American Journal of Botany 89(3): 460-465.
#' @export

genetic_structure <- function( x, stratum="Population", mode=c("Gst", "Gst_prime", "Dest")[1], loci="All", nperm=0, verbose=TRUE ) {
  
  if( !inherits(x,"data.frame") )
      stop("You need to pass a data frame to the funciton genetic_structure()...")
    
  if( !(stratum %in% names(x) ) ) 
    stop("You must specify which stratum to use for the estimation of genetic structure.")

  if( length(loci) == 1 && loci=="All" )
    loci <- column_class( x, "locus" )
  
  else if( any(setdiff(loci,column_class(x,"locus"))) )
    stop("Some of the loci requested are not in the data.frame")
  
  strata <- x[[stratum]]
  
  ret <- list()
  class(ret) <- "genetic_structure"
  ret$mode <- mode
  ret$loci <- list()

  if( verbose )
    cat(paste("Estimating",mode,": "))
  
  if( mode == "Gst") 
    for( locus in loci ) {
      if( verbose ) cat(".")
      ret$loci[[locus]] <- Gst(x[[stratum]],x[[locus]])
      ret$loci[[locus]]$locus <- locus
    }
  
  else if( mode == "Gst_prime") 
    for( locus in loci ) {
      if( verbose )cat(".")
      ret$loci[[locus]] <- Gst_prime(x[[stratum]],x[[locus]])
      ret$loci[[locus]]$locus <- locus
    }
  
  else if( mode == "Dest" ) 
    for( locus in loci ) {
      if( verbose )cat(".")
      ret$loci[[locus]] <- Dest(x[[stratum]],x[[locus]])
      ret$loci[[locus]]$locus <- locus
    }
  
  else 
    stop( paste( "This function does not support the parameter '",mode,"', see the documentation for supported types" ) )
  
  if( verbose )
    cat("\n")
  
  return( ret )
}



print.genetic_structure <- function( x, ... ){
  
  cat("Genetic Structure\n")
  print(as.data.frame.genetic_structure(x))
  
}


