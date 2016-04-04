#' Test script for genetic distance
#' 


data(arapat)


x <- arapat
stratum <- "Population"


# make output
pops <- levels( x[[stratum]] )
K <- length(pops)
D <- matrix(0,nrow=K,ncol=K)
r <- length( column_class(x,"locus"))

freqs <- frequency_matrix(x,stratum=stratum)
freqs <- as.matrix( freqs[,2:ncol(freqs)] )


z <- seq(K)
idx <- cbind( row = unlist(lapply(2:K, function(x) x:K), use.names = FALSE), 
              col = rep(z[-length(z)], times = rev(tail(z, -1))-1))


foo <- function( i, freqs, r){
  x <- sum(freqs[i[1],]^2/r)
  y <- sum(freqs[i[2],]^2/r)
  xy <- sum( (freqs[i[1],]*freqs[i[2],])/r )
  return( -log( xy/(x*y) ) )
}

nei <- apply(idx,1,foo, freqs=freqs, r=r)
m <- matrix( 0, K, K)
m[idx[,1],idx[,2]] <- nei


