#' Estimation Nei's Gst parameter with Hedrick's correction for allelic diversity
#' 
#' This function estimates the parameter (and potentially the confidence
#'  surrounding its value) for Gst.  It is corrected by the diversity of the 
#'  parameter as outlined by Hedrick.
#' @param x  A vector of \code{\link{locus}} objects or a \code{data.frame} with \code{locus} objects.
#' @param stratum Either a vector of strata variables if \code{x} is a \code{locus} vector or 
#'  the name of the column representing strata in \code{x} if it is a \code{data.frame}.
#' @param nperm The number of permutations to run for significance of the
#'  estimator.
#' @param size.correct A flag indicating that the estimate should be corrected for
#'  based upon sample sizes (default=TRUE).
#' @return An \code{data.frame} with Gst, Ht, and Hs and optionally P.  If more than one locus is provided,
#'  then a 'mutlilocus' estimate is shown using the harmonic mean of individual locus Gst_prime values.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#'  a1 <- sample( LETTERS[1:5], size=20, replace=TRUE)
#'  a2 <- sample( LETTERS[4:8], size=20, replace=TRUE)
#'  raw_alleles <- matrix( c(a1,a2), ncol=2, byrow=TRUE )
#'  locus <- locus( raw_alleles, type="column")
#'  Population <- c(rep("Pop-A",10),rep("Pop-B",10))
#'  Gst_prime( locus, Population )
#'  a1 <- sample( LETTERS[1:5], size=20, replace=TRUE)
#'  a2 <- sample( LETTERS[4:8], size=20, replace=TRUE)
#'  raw_alleles <- matrix( c(a1,a2), ncol=2, byrow=TRUE )
#'  locus2 <- locus( raw_alleles, type="column")
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  Gst_prime( df, nperm=99)
Gst_prime <- function(  x, stratum="Population",  nperm=0, size.correct=TRUE ) {
  
  # Do this function recursively if a data.frame is passed as loci
  if( is(x,"data.frame") ) {
    
    locus_names <- column_class(x,"locus")
    
    if( length(locus_names)==0 )
      stop("You must pass some loci to this function")
    
    if( !(stratum %in% names(x)) )
      stop("If you pass a data.frame to Gst(), you need to indicate a stratum varaible column.")
    
    strata <- factor(as.character(x[[stratum]]))
    k <- length(levels(stratum))
    K <- length(locus_names)
    ret <- data.frame(Locus=locus_names, Gst=numeric(K), Hs=numeric(K), Ht=numeric(K), P=numeric(K), stringsAsFactors=FALSE)
    
    for( i in 1:length(locus_names) ){
      data <- x[[locus_names[i]]] 
      r <- Gst_prime( data, strata, nperm, size.correct)
      ret[i,2:5] <- r
      
    }
    
    if( length( locus_names) > 1 ) {
      
      #
      # TODO Determine if these can ever be differents length vectors?
      #
      Hs.tot <- 1.0 / mean( 1/ret$Hs ,na.rm=TRUE)
      Ht.tot <- 1.0 / mean( 1/ret$Ht ,na.rm=TRUE)
      
      # do the multiloucs as a summation
      #if( size.correct ) {
      #  n.harmonic <- 1/mean(1/table(stratum))
      #  hs.estimated <- (2*n.harmonic)/(2*n.harmonic -1) * Hs.tot
      #  ht.estimated <- Ht.tot + hs.estimated/(2*k*n.harmonic)    
      #  Gst.tot <- ((1-hs.estimated/ht.estimated)*(k-1+hs.estimated) )
      #  Gst.tot <- Gst.tot / ((k-1)*(1-hs.estimated))
      #}
      #else {
      x <- ret$Gst
      x[ x < 0 ] <- NA
        Gst.tot <- 1.0 / mean( 1/x ,na.rm=TRUE)
      #} 
      
      ret[K+1,1] <- "Multilocus"
      ret[K+1,2] <- Gst.tot
      ret[K+1,3] <- Hs.tot
      ret[K+1,4] <- Ht.tot      
    }
    
  }
  
  # do this for a single locus
  else {

    if( !is(x,"locus") )
      stop("This function requires objects of type 'locus' to function.")
    
    if( missing( stratum) )
      stop("You need to specify strata for Gst_prime to be estimated.")
    
    if( length(x) != length(stratum))
      stop("If you pass vectors to Gst_prime() for loci and stratum, they must be the same size...")
    
    if( !is(stratum,"factor") )
      stratum <- factor( stratum)
    
    k <- length(levels(stratum))
    strata.lvls <- levels(stratum)
    inds <- to_mv.locus( x )
    p.vec <- colSums(inds)
    ht <- 1-sum((p.vec/sum(p.vec))^2, na.rm=TRUE)
    hs <- mean(unlist(lapply( strata.lvls, 
                              function(strat,inds,strata ) {
                                s <- colSums(as.matrix(inds[strata==strat,]))
                                f <- 1-sum((s/sum(s))^2)
                                return(f)
                              }, inds=inds, strata=stratum)), na.rm=TRUE)
    
    if( size.correct ) {
      n.harmonic <- 1/mean(1/table(stratum))
      hs.estimated <- (2*n.harmonic)/(2*n.harmonic -1) * hs
      ht.estimated <- ht + hs.estimated/(2*k*n.harmonic)    
      Gst_prime <- ((1-hs.estimated/ht.estimated)*(k-1+hs.estimated) )
      Gst_prime <- Gst_prime / ((k-1)*(1-hs.estimated))
    }
    else {
      hs.estimated <- hs
      ht.estimated <- ht
      Gst_prime <- (1-hs/ht) * (k-1+hs)
      Gst_prime <- Gst_prime / ((k-1)*(1-hs))
    }
    
    if( nperm > 0 ) {
      perms <- rep(NA,nperm)
      for(i in 1:nperm ) {
        hs.perm <- lapply( strata.lvls, 
                           function(strat,inds,strata ) {
                             s <- colSums(as.matrix(inds[strata==strat,]) )
                             f <- 1 - sum((s/sum(s))^2) 
                           }, 
                           inds=inds, 
                           strata=sample(stratum))
        perms[i] <- mean(unlist(hs.perm), na.rm=TRUE)
      }
      if( size.correct ) {
        perms <- (2*n.harmonic) / (2*n.harmonic-1) * perms
        Gst_prime.perm <- ((1-perms/ht.estimated)*(k-1+perms) )
        Gst_prime.perm <- Gst_prime.perm / ((k-1)*(1-perms))      
      }
      else {
        Gst_prime.perm <- (1-perms/ht) * (k-1+perms)
        Gst_prime.perm <- Gst_prime.perm / ((k-1)*(1-perms))
      }
      Gst_prime.perm <- Gst_prime.perm[ !is.na( Gst_prime.perm) ]
      P <- sum( Gst_prime.perm >= Gst_prime ) / length( Gst_prime.perm )
    }
    else 
      P <- 0
    
    ret <- data.frame( Gst=Gst_prime, Hs=hs.estimated, Ht=ht.estimated, P=P)
    
  
  }
  
  
  
  return( ret )
}


