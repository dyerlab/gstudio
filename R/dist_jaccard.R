#' Estimation of jaccard distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the Jaccard set distance metric.  
#' @param x A \code{data.frame} with both stratum and \code{locus} 
#'  objects in them.
#' @param stratum The name of the stratum variable in \code{x}
#' @return A matrix of Jaccard distance
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#'   AA <- locus( c("A","A") )
#'   AB <- locus( c("A","B") )
#'   AC <- locus( c("A","C") )
#'   BB <- locus( c("B","B") )
#'   BC <- locus( c("B","C") )
#'   CC <- locus( c("C","C") )
#'   loci <- c(AA,AA,AB,AA,BB,BC,CC,BB,BB,CC)
#'   df <- data.frame( Population=c(rep("A",5),rep("B",5) ), TPI=loci )
#'   dist_jaccard(df)
dist_jaccard <- function( x, stratum="Population" ) {
  
  if( !is( x, "data.frame") )
    stop("You need to pass a data.frame to dist_cavalli() to work.")
  
  if( !(stratum %in% names(x)))
    stop("You need to specify the correct stratum for dist_cavalli() to work.")
  
  locus_names <- column_class( x, "locus")
  K <- length( locus_names )
  if( K==0)
    stop("You need to pass objects of type 'locus' to use for dist_cavalli().")
  else if( K > 1 )
    message("Jaccard distance will be assumed to be entirely additive across loci.")
  
#   pops <- partition(x, stratum=stratum) 
#   K <- length(pops)
#   
#   ret <- matrix(0,ncol=K,nrow=K)
#   colnames(ret) <- rownames(ret) <- names(pops)
# 
#   #d <- to_mv( x )
#   
#   for(locus in locus_names){
#     for(i in 1:K ){
#       p1 <- alleles( pops[[i]][[locus]], all=FALSE)
#       for( j in i:K){
#         if(i!=j){
#           p2 <- alleles( pops[[j]][[locus]], all=FALSE)
#           m11 <- length(intersect( p1, p2 ))
#           m01 <- length(setdiff( p2, p1 ))
#           m10 <- length(setdiff( p1, p2 ))
#           jc <- (m01 + m10) / (m01 + m10 + m11)
#           ret[i,j] <- ret[i,j] + jc
#         }
#       }
#     }   
#   }  
#  ret <- ret + t(ret)
#  ret <- ret / length(locus_names)

  ret <- suppressMessages( dist_bray(x,stratum) )
  ret <- (2*ret) / (1+ret)
    
  return(ret)

}
