#' Makes a random population
#' 
#' This function takes a \code{frequencies()} object and
#'  returns a \code{data.frame} with the appropriate 
#'  stratum and loci derived randomly.
#' @param x A \code{data.frame} as returned by the \code{frequencies()}
#'  funciton.  For a single locus, it has columns "Allele" and "Frequency"
#'  for multiple loci, it has a "Locus" column, and if it is to be 
#'  subdivided into strata, you have a "Strata" column.
#' @param N The number of individuals to create (default = 20).  This is a 
#'  'per-stratum' estimate so if you have K strata the result will be K*N 
#'  individuals.
#' @param F The inbreeding index (Fis).
#' @return A \code{data.frame} with individuals, loci, and potentially strata
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#'   freqs <- c(0.55, 0.30, 0.15, 0.34, 0.34, 0.32)
#'   loci <- c(rep("TPI",3), rep("PGM",3))
#'   alleles <- c(LETTERS[1:3],LETTERS[8:10])
#'   f <- data.frame(Locus=loci, Allele=alleles, Frequency=freqs)
#'   make_population(f,N=20)
make_population <- function( x, N=20, F = 0){
  
  # subdivided
  if( "Stratum" %in% names(x) ){
    strata <- levels( factor( x$Stratum ))
    K <- length(strata)
    ret <- data.frame()
    for( stratum in strata ){
      f <- x[x$Stratum==stratum, c(2:4)]
      r <- make_population( f, N, F )
      l <- length(names(r))
      r$Population =stratum
      r <- r[, c( (l+1), (1:l))]
      ret <- rbind( ret, r)
    }
    ret$Population <- factor( ret$Population )
  }
  
  else if( "Locus" %in% names(x) ){
    loci <- levels( factor( x$Locus) )
    ret <- data.frame(ID=1:N) 
    for( locus in loci ){
      f <- x[x$Locus == locus,c(2,3)]
      theLoc <- make_population(f,N,F)
      ret[[locus]] <- theLoc        
    }
  }
  
  else if( all(c("Allele","Frequency") %in% names(x) )) {
    return( make_loci(x,N,F) )    
#     alleles <- x$Allele
#     freqs <- x$Frequency
#     if( sum(freqs) != 1){
#       d <- (1.0 - sum(freqs))/length(alleles)
#       warning( paste( "Your allele frequencies do not add to 1.0.  The difference of ",
#                       d, " will be partioned across all noted alleles", sep=""))
#       freqs <- freqs + d
#     }
#     K <- length(alleles)
#     P <- rep(0,(K + K*(K-1)/2) )
#     G <- rep("",length(P))
#     ctr <- 1
#     for( i in 1:K){
#       for( j in i:K){
#         G[ctr] <- paste(alleles[i],alleles[j],sep=":")
#         if( i==j ) {
#           P[ctr] <- (freqs[i]^2)*(1-F) + (F * freqs[i])
#         }
#         else
#           P[ctr] <- (2*freqs[i] * freqs[j]) * (1-F) 
#         ctr <- ctr+1
#       }
#     }
#     
#     P <- round(P*N)
#     loci <- c()
#     for( i in 1:length(P)){
#       loci <- c( loci, rep(G[i],times=P[i]))
#     }
#     loci <- locus( loci, type="separated")
#     ret <- data.frame(ID=1:N, Locus=loci)
#     print("Hello?")
#     print(P)
#     print(G)
  }
  else
    stop("Unrecognized data.frame being passed to make_population().")
           
  return(ret)
}

