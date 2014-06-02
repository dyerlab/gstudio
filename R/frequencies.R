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
#' @note You can submit RAD-seq genotypes in a normal \code{data.frame} 
#'  to this function BUT it must be formatted as follows.  Each locus
#'  is represented by three columns of probabilities (must sum to 1).
#'  You must label the columns of your data frame with the name of the locus 
#'  and a column number separated by an underscore '_' (e.g., Loc1_1, Loc1_2, 
#'  Loc1_3, Loc2_1, Loc2_2, Loc2_3, etc).  The function will name the locus 
#'  the part before the dash (e.g., Loc1). The frequencies of the heterozygote 
#'  genotype frequency represented in the middle position.
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
#' # RAD-seq example
#' m <- matrix(abs(rnorm(30)),nrow=10)
#' m <- m / rowSums(m)
#' df <- data.frame(m)
#' names(df) <- c("Loc1_1","Loc1_2","Loc1_3")
#' frequencies(df)
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


#' A private method 
.frequencies.snp.prob <- function( df, loci, stratum, ... ){
  colnames <- names(df)
  loc_cols <- colnames[ unlist(lapply( colnames, function(x) return( length(strsplit(x,split="_")[[1]])==2))) ]
  loc_names <- unique(matrix(unlist(strsplit(loc_cols,"_")),ncol=2,byrow=TRUE)[,1])
  
  if( !length(loc_names) )
    stop("If you are going to estimate frequencies from RAD-seq data, you must label your loci as XXX_Y where the XXX is the name of the snp locus.  These must be unique.")

  
  # the no stratum frequency
  if( missing(stratum) ) {

    ret <- data.frame( Locus=rep(loc_names,each=2), Allele=rep( c("A","B"), times=length(loc_names)), Frequency=0, stringsAsFactors=FALSE)
    x <- df[,loc_cols]
    if( ncol(x) %% 3 )
      stop("You must have three columns for each locus representing the probability of each genotype.")
    N <- nrow(x)
    
    for( i in seq(1,ncol(x),by = 3)) {
      theloc <- strsplit(loc_cols[i],split="_")[[1]][1]
      f.a <- sum( 2*x[,i] )/(2*N) + sum( x[,(i+1)])/(2*N)
      ret$Frequency[ (ret$Locus == theloc & ret$Allele=="A") ] <- f.a
      ret$Frequency[ (ret$Locus == theloc & ret$Allele=="B") ] <- 1-f.a
    }
  }
  else {
    pops <- partition(df,stratum = stratum)
    ret <- data.frame( Stratum=character(0), Locus=character(0), Allele=character(0), Frequency=numeric(0) )
    for( strat in names(pops) ){
      d <- .frequencies.snp.prob( pops[[strat]] )
      d$Stratum <- strat
      d <- d[, c(4,1,2,3)]
      ret <- rbind( ret, d )
    }
  }
  
  return(ret)
}

#' @return A data frame with Frequencies, Alleles, Loci, and perhaps 
#'  Stratum columns (Allele and Frquencies are at a minimium).
#' @method frequencies data.frame
#' @rdname frequencies
#' @export
frequencies.data.frame <- function( x, loci, stratum, ... ) {

  # no loci specified so grab all of them
  if( missing(loci) || is.na(loci) ) {
    loci <- column_class( x, "locus" )
  }
  
  # no 'locus' objects, try to send it to the snp.prob 
  if( length(loci)==1 & any(is.na(loci) ))
    return( .frequencies.snp.prob( x, stratum ) )
  
  # throw error if asked for non-existent loci
  if( length( setdiff( loci, column_class(x,"locus") )) ){
    loci <- intersect( column_class( x, "locus" ), loci)
    stop("Asked for loci not in the data frame...")
  }
  
  # all loci to do.
  if( missing( stratum ) ){
    ret <- data.frame( Locus=character(0), Allele=character(0), Frequency=numeric(0), stringsAsFactors=FALSE)
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










