#' Returns multilocus assignment probability
#' 
#' This function takes one or more individuals and estimates
#' their probability of coming from indiviudal populations
#' from multilocus genotype frequencies.
#' @param individual A \code{data.frame} with a single row for an indiviudal with one or 
#'  more \code{locus} objects
#' @param frequencies
#' 

multilocus_assignment <- function( individual, frequencies, verbose=FALSE ) {
  
  # Check to see correct type of data passed
  if( !is(individual,"data.frame") || length( column_class(individual,"locus")) < 1 )
    stop("You must pass a data frame of individuals with locus columns to this function")
  if( nrow(individual) > 1 ) {
    warning("Only the first indiviudal will be used.")
    indiviudal <- individual[1,]
  }
  
  if( !is( frequencies, "data.frame") || !(all( c("Stratum","Locus","Allele","Frequency") %in% names(frequencies))))
    stop("You must pass a data.frame from frequencies() estimated among strata")
  
  loci <- column_class(individual,"locus")
  if( !setequal(loci,unique(frequencies$Locus)) ){
    stop("Using a subset of loci, individuals and frequencies are out of sync.")
  }
  
  pops <- sort(unique(frequencies$Stratum))
  ret <- data.frame(Stratum=pops, Probability = 0 )
  has_missing <- FALSE
  for( pop in pops) {
    prob <- 1
    
    if( verbose ) 
      print(paste("######################################################   POPULATION:",pop))
    if( prob > 0 ) {
      for( locus in loci ) {
        popfreq <- frequencies[ frequencies$Stratum == pop & frequencies$Locus==locus ,]
        loc <- individual[[locus]]
        
        if( !is.na(loc)){
          all_alleles <- alleles(loc)
          
          if( all(all_alleles %in% popfreq$Allele ) ) {
            f <- prod(unlist(lapply( all_alleles, function(x) return( popfreq$Frequency[ popfreq$Allele==x]))))
            if( is_heterozygote(loc) )
              f <- f*2
          }
          else {
            f <- prob <- 0
            if( verbose )
              cat("-----------------------------------------------Excluded from",pop,"at locus",loc,"\n")
          }
          
        }
        else {
          has_missing <- TRUE
        }
        
        if( verbose ){
          print(paste("Locus: ",locus, f))
          print(popfreq)
        }
        
        
        # bail if you have a zero 
        prob <- prob * f
        
        if( verbose )
          cat( loc, popfreq$Allele, popfreq$Frequency, f, "\n")    
      }
      
    }
    
    # assign probability
    ret$Probability[ ret$Stratum == pop ] <- prob
  }
  
  ret <- ret[order(-ret$Probability),]
  
  if( has_missing ){
    warning("This individual has missing genotypes, cannot compre assignment probability to indiviudals who do not have missing data.")
  }
  
  if( !verbose )
    ret <- ret[ ret$Probability > 0,]
  
  ret$Posterior <- ret$Probability / sum( ret$Probability )
  
  return( ret )
  
}