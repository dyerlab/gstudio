#' Estimate Individual Heterozygosity
#'  
#' Returns the heterozygosity of loci within each individual.  This is a per-individual estiamte
#' @param x A \code{data.frame} that has rows of individuals who have one or more \code{locus} 
#' columns
#' @return The average heterozygosity for non-missing loci per individual as a \code{data.frame} 
#'  object with all non-locus columns identical to those passed.
#' @note This function must be called on a \code{data.frame} of individuals, it will assume that
#'  each row contains at least one column of type \code{locus}
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' loc1 <- c( locus( c("A","A") ), locus( c("A","B") ), locus( c("A","B") ) )
#' loc2 <- c( locus( c("A","B") ), locus( c("A","A") ), locus( c("A","B") ) )
#' loc3 <- c( locus( c("A","B") ), locus( c("A","A") ), locus( c("A","B") ) )
#' loc4 <- c( locus( c("A","A") ), locus( c("A","A") ), NA )
#' pop <- c("A","A","B")
#' df <- data.frame( ID = 1:3, pop, loc1, loc2, loc3, loc4 )
#' Hi( df )
Hi <- function( x ) {  

  if(!is( x, "data.frame") ) {
    stop("please pass a data.frame object to this function.")
  }

  locus_names <- column_class(x,class="locus")
  if( length(locus_names)==0) {
    stop("Cannot estimate expected heterozygosity if there are no loci...")
  }
  
  ret <- length( locus_names) - apply( apply( x[,locus_names],1, is.na), 2, sum )  
  
  for( i in 1:nrow(x)){ 
    row <- x[i,locus_names]
    nHet <- sum(is_heterozygote(locus(as.vector(unlist( row )), type="separated") ))
    if( ret[i] > 0 ) { 
      ret[i] <- nHet/ret[i]
    }
  }
  
  df <- x 
  df$Hi <- ret 
  df[,locus_names] <- NULL 
  
  return( df ) 
}



