#' Genotype Frequencies
#' 
#' Provides a summary of gentoype frequencies (observed and expected) from
#'  a vector of \code{locus} objects.
#' @param x An object of type \code{locus}
#' @return A \code{data.frame} with genotype, observed, and expected as counts.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' @examples
#'   freqs <- c(0.55, 0.30, 0.15)
#'   alleles <- c(LETTERS[1:3])
#'   f <- data.frame(Locus="PGM", Allele=alleles, Frequency=freqs)
#'   data <- make_population(f,N=20)
#'   table(data$PGM)
#'   genotype_frequencies( data$PGM )
genotype_frequencies <- function( x ) {
  if( missing(x))
    stop("You need to pass this function a vector of locus objects.")
  
  if( is(x,"data.frame")){
    ret <- data.frame( Locus=NA, Genotype=NA, Observed=NA, Expected=0,stringsAsFactors=FALSE)
    
    for( locus_name in column_class(x,"locus")){
      theLoc <- x[[locus_name]]
      df <- genotype_frequencies(theLoc)
      df$Locus <- locus_name
      ret <- rbind(ret,df)
    }
    
    ret <- ret[ !is.na(ret$Locus),]
    return( ret )
    
  }
  else if( !is(x,"locus")) {
    stop("This function works on locus objects only")
  }
    
  
  # remove missing data
  x <- x[ !is.na(x)]
  
  t <- table( x )
  ret <- data.frame( Genotype=names(t), Observed=as.numeric(t), Expected=0,stringsAsFactors=FALSE)
  f <- frequencies( x ) 
  expected.freq <- f$Frequency %*% t(f$Frequency)
  
  for( i in 1:nrow(f)){
    for( j in i:nrow(f)){
      g <- paste(sort(c(f$Allele[i],f$Allele[j])),collapse=":")
      if( !(g %in% ret$Genotype ) )
        ret <- rbind( ret, data.frame(Genotype=g,Observed=0,Expected=0,stringsAsFactors=FALSE))
      fe <- f$Frequency[i]*f$Frequency[j]
      if(i!=j)
        fe <- 2 * fe
      ret$Expected[ ret$Genotype==g]  <- fe
    }
  }
  
  ret$Expected <- ret$Expected * length(x)
  
  if( any( ret$Expected < 5.0 ))
    warning("Some genotype expectations are < 5, a continuity correction should be applied.  See ?hwe")
  
  return( ret )
}

