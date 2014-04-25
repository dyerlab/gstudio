#' Estimation of amova distance
#' 
#' This function returns a measure of genetic distance based upon
#'  the AMOVA distance metric.  
#' @param x A \code{data.frame} with \code{locus} columns in it.
#' @return The AMOVA distance matrix
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#' AA <- locus( c("A","A") )
#' AB <- locus( c("A","B") )
#' BB <- locus( c("B","B") )
#' AC <- locus( c("A","C") )
#' AD <- locus( c("A","D") )
#' BC <- locus( c("B","C") )
#' BD <- locus( c("B","D") )
#' CC <- locus( c("C","C") )
#' CD <- locus( c("C","D") )
#' DD <- locus( c("D","D") )
#' loci <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD) 
#' D <- dist_amova( loci )
#' rownames(D) <- colnames(D) <- as.character(loci)
#' D
dist_amova <- function( x ) {
  
  if( is(x,"locus") )
    x <- data.frame( LOCUS=x )
  if( !is(x,"data.frame") )
    stop(paste("The function dist_amova() requires a locus vector or a data frame of locus vectors.  You passed a '",class(x), "' object.",sep=""))
  
  N <- dim(x)[1]
  ret <- matrix(0, ncol=N,nrow=N)
  
  
  # check for amova with mixed ploidy
  p <- ploidy(x)
  
  # make data vectors for 2gener amova
  if( any( p$Ploidy != round(p$Ploidy))){
    data <- FALSE
    
    if( any( p$Ploidy > 2))
      stop("As currently implemented, the 2gener amova distance is limited to diploid individuals.")
    
    loci <- p$Locus
    
    for( locus_name in loci){
      locus <- x[[locus_name]]
      freqs <- frequencies(locus)
      y <- to_mv( locus )
      p <- ploidy(locus)
      
      for( i in seq(1:N)[p==2]){  
#         a <- colnames(y)[y[i,]!=0]
#         f <- freqs$Frequency[ freqs$Allele %in% a]
#         idx <- which(names(y[i,]) %in% a)
#         y[i,idx] <- (y[i,idx]) * f / sum(f)
        f <- freqs$Frequency * y[i,]
        y[i,] <- f/sum(f)/2
        
      }
      
      if( is(data,"logical") )
        data <- y
      else
        data <- cbind( data, y)
    }
    
  }
  
  # make data vectors as adult AMOVA
  else 
    data <- to_mv( x, drop.allele=FALSE )
  
  
  for( i in 1:N) {
    x <- data[i,]
    for( j in 1:i) {
      if( i != j ) {
        y <- data[j,]
        ret[i,j] <- ret[j,i] <- sum( 2*t(x-y) %*% (x-y) )
      }
    }
  }
  
  
  
  
  return( ret ) 
}
