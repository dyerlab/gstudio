#' Get allele frequencies
#' 
#' This function converts loci into data frames consisting of
#'  alleles, stratum, frequencies, etc.
#' @param x Either a vector of types \code{locus} or a \code{data.frame}
#'  containing \code{locus} objects.
#' @param loci The loci to get frquencies from (default is all).
#' @param stratum Partition of the data based upon this stratum
#'  (default is no partitions).
#' @param ... Ignored
#' @return A data frame with Frequencies, Alleles, Loci, and perhaps 
#'  Stratum columns (Allele and Frquencies are at a minimium).
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#' AA <- locus( c("A","A") )
#' AB <- locus( c("A","B") )
#' BB <- locus( c("B","B") )
#' loc1 <- c(AA,AB,AB,AA,BB)
#' frequencies(loc1)
#' loc2 <- c(BB,BB,AB,AA,BB)
#' df <- data.frame(Population=c(rep("A",3), rep("B",2)), TPI=loc1, PGM=loc2)
#' frequencies(df)
#' frequencies(df,stratum="Population")
frequencies <- function( x, loci, stratum, ... ) {
  UseMethod("frequencies")
}

#' @return A data frame with Frequencies, Alleles, Loci, and perhaps 
#'  Stratum columns (Allele and Frquencies are at a minimium).
#' @method frequencies default
#' @rdname frequencies
#' @export
frequencies.default <- function( x, ... ) {
  t <- table(x)
  Allele <- as.character(names(t))
  Frequency <- as.numeric(t)/sum(t)
  ret <- data.frame( Allele, Frequency )
  return( ret )
}


#' @return A data frame with Frequencies, Alleles, Loci, and perhaps 
#'  Stratum columns (Allele and Frquencies are at a minimium).
#' @method frequencies locus
#' @rdname frequencies
#' @export
frequencies.locus <- function( x, loci, ... ) {
  ret <- frequencies.default( alleles( x ) )
  ret$Allele <- as.character( ret$Allele)
  return( ret )
}


#' @return A data frame with Frequencies, Alleles, Loci, and perhaps 
#'  Stratum columns (Allele and Frquencies are at a minimium).
#' @method frequencies data.frame
#' @rdname frequencies
#' @export
frequencies.data.frame <- function( x, loci, stratum, ... ) {
  
  # no loci specified so grab all of them
  if( missing(loci) ) {
    loci <- column_class( x, "locus" )
  }
  
  # throw error if asked for non-existent loci
  if( length( setdiff( loci, column_class(x,"locus") )) ){
    loci <- intersect( column_class( x, "locus" ), loci)
    stop("Asked for loci not in the data frame...")
  }
  
  # all loci to do.
  if( missing( stratum ) ){
    ret <- data.frame( Locus=character(0), Allele=character(0), Frequency=numeric(0) )
    for( locus in loci ) {
      loc <- frequencies.locus( x[[locus]] )
      if( nrow(loc) ) {
        loc$Locus <- locus
        ret <- rbind( ret, loc[,c(3,1,2)] )
      }
    }
  }
  else if (!(stratum %in% names(x))){
    stop("Asking for non-existant stratum.")
  }
  
  # Asking for stratum
  else {
    ret <- data.frame( Stratum=character(0), Locus=character(0), Allele=character(0), Frequency=numeric(0))
    pops <- partition( x, stratum=stratum )
    popnames <- names(pops)
    for( pop in popnames ){
      
      strat <- frequencies( pops[[pop]], loci )
      
      if( nrow(strat) ){
        strat$Stratum <- pop
        ret <- rbind( ret, strat[,c(4,1,2,3)] )        
      }
    }
  }
  
  return( ret )
}










