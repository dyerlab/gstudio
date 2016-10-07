#' Returns counts of alleles at a locus by population
#' 
#' Takes a locus and set of populations and returns matrix of
#'   allele counts
#' @param x A \code{data.frame} object with loci and stratum in it.
#' @param locus The name of the locus to use
#' @param stratum The stratum to partition by.
#' @return A matrix where each row is a strata and each column is a count of alleles
#' @importFrom reshape2 dcast
#' @export
#' @author Rodney J. Dyer 
#' 
allele_counts <- function( x, locus, stratum="Population") {
  if( !is(x,"data.frame"))
    stop("You must pass a data.frame object to the allele_counts function")
  if( missing(locus) || !(locus %in% names(x)))
    stop("You must specify the locus name to use")
  if( !(stratum %in% names(x)))
    stop("You must indicate the stratum column")
  
  # find allele output
  alleles <- unique( as.vector( alleles( x[[locus]]) )  )
  alleles <- alleles[ !is.na(alleles) ]
  
  L <- length( alleles )
  pops <- partition( x, stratum)
  K <- length(pops)
  
  ret <- data.frame( Stratum=NA,
                     Alleles=NA,
                     Counts=NA)
  
  for( i in 1:K){
    pop <- pops[[i]]
    loc <- pop[[locus]]
    cts <- data.frame( Stratum=names(pops)[i], Alleles=alleles, Counts=0)
    if( !(all(is.na(loc)))){
      v <- table( alleles(loc) )      
      cts <- data.frame( Stratum=names(pops)[i], Alleles=names(v), Counts=as.numeric(v))
    }
    ret <- rbind( ret, cts )
  }
  ret <- ret[ !is.na(ret$Stratum),]
  ret <- dcast(ret, Stratum~Alleles,fill = 0, value.var = "Counts")
  return( ret )
}