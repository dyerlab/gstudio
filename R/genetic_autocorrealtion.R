#' Performs 'Autocorrelation Analysis
#' 
#' This function takes two distance matrices, one for physical separation
#'  and the other for genetic separation as well as a bin size and performs
#'  the spatial autocorrelation analysis of Smouse & Peakall (1999).
#' @param P A square physical distance matrix
#' @param G A square genetic distance matrix (same size as P)
#' @param bins The distance bins to be estimated.  These will be taken sequentially 
#'  such that the first bin will be all observations whose separation is greater than
#'  binsize[1] and less than binsize[2].
#' @param perms The number of permutations to use to test significance
#'  at each distance bin.
#' @param plot A flag (default=FALSE) to show distribution of randomized 
#'  correlations.
#' @return A data.frame with bins and correlations.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
genetic_autocorrelation <- function( P, G, bins, perms=0, plot=FALSE ){
  Bin <- To <- NA
  if( missing(P) || missing(G) || missing(bins)){
    stop("Need P, G, and binsize for autocorrelation.")
  }
  if( any( dim(P) != dim(G)) ){
    stop("Need both P and G to have the same dimensionality for genetic_autocorrelation()")
  }
  ret <- NULL
  
  
  C <- dist2cov(G)
  ret <- data.frame(From=bins[1:(length(bins)-1)], 
                    To=bins[2:length(bins)], 
                    R=NA,
                    N=NA,
                    P=NA)
  
  if( perms > 0 ){
    R <- data.frame(Bin=NA,R=NA)
  }  
  
  for( lag in 1:nrow(ret) ) {
    X <- P
    X[ X < ret$From[lag] ] <- 0
    X[ X >= ret$To[lag] ] <- 0
    X[ X != 0 ] <- 1
    diag(X) <- rowSums(X)
    dX <- diag(X)
    dC <- diag(C)
    rbot <- sum( dX*dC )
    rtop <- sum( C * X ) - rbot
    r <- rtop/rbot
    ret$R[lag] <- r
    ret$N[lag] <- sum( X[lower.tri(X)])
    if( perms > 0 ) {
      p <- rep(0,perms)
      for(i in 1:perms){
        c <- permute_matrix(C)
        rbot <- sum( diag(c) * dX )
        rtop <- sum( c * X ) - rbot
        p[i] <- rtop/rbot
      }
      ret$P[lag] <- sum( p>= r)/perms
      
      # add to R matrix
      d <- data.frame( Bin=ret$To[lag],
                       R=p)
      R <- rbind( R, d )
    }
  }
  
  if( perms > 0 & plot==TRUE ){
    R <- R[ !is.na(R$Bin),]
    p <- ggplot() + geom_violin(aes(x=Bin,y=R, group=Bin),data=R, trim=FALSE) +
      geom_line( aes(x=To,y=R),data=ret)
    print(p) # print method for ggplot object; appropriate here
  }
  
  
  
  
  
  return( ret )
}

