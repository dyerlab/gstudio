#' This function returns the kronecker operator for two loci.
#' 
#' This is a quick utility function that provides the Kronecker operators
#'  for a pair of loci
#' @note The Kronecker operator is a vector consisting of values related
#'  to the similarity of alleles among two loci.  For the genotypes AiAj and 
#'  AkAl the Kronecker deltas are =0 of the subscripted alleles are different
#'  and =1 if they are the same (e.g., dij = 0 if heterzygote, = 1 if homozygote)
#' @param locus1 An object of type 'locus'
#' @param locus2 An object of type 'locus' 
#' @return a named vector of kronecker products.
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' loc1 <- locus( c(1,1) )
#' loc2 <- locus( c(1,2) )
#' loc3 <- locus( c(2,2) )
#' kronecker_delta( loc1, loc2 )
#' kronecker_delta( loc1, loc3 )
#' kronecker_delta( loc2, loc3 )
kronecker_delta <- function( locus1, locus2 ) {
  ret <- rep(0,6)
  names(ret) <- c("dab","dac","dad","dbc","dbd","dcd")
  
  if( missing( locus1 ) | missing( locus2 ) ) {
    stop( "Cannot estimate kronecker operators on missing data")
    return( ret )
  }
  
  if( !is(locus1,"locus") | !is(locus2,"locus"))
    stop("You need to pass two locus objects to this function.")
  
  if( length( locus1 )>1 | length( locus2 )>1 )
    stop("Cannot estimate kronecker operators on vectors, it is a pair-wise transform.")
  
  loc1 <- alleles( locus1 )
  loc2 <- alleles( locus2 )
 
  if( !is_heterozygote(locus1 ))
    ret[1] <- 1
  if( loc1[1]==loc2[1] )
    ret[2] <- 1
  if( loc1[1]==loc2[2] )
    ret[3] <- 1
  if( loc1[2]==loc2[1] )
    ret[4] <- 1
  if( loc1[2]==loc2[2] )
    ret[5] <- 1
  if( !is_heterozygote( locus2))
    ret[6] <- 1
    
  return( ret )
}