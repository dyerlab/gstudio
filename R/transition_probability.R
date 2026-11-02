#' Returns transition probability for offspring given one or more parents.
#' 
#' This is a quick function used in paternity testing that provides the
#'  probability of transition for a locus of any ploidy level.
#'
#' With both parents, the value is the Mendelian probability of the offspring
#'  genotype from that cross.  With one parent and \code{freqs}, it is the
#'  probability of the offspring given that parent and a random mate drawn
#'  from the population, the sum over the parent's alleles shared with the
#'  offspring of (probability the parent transmits it) times (frequency of
#'  the offspring's other allele).  The ratio of the two is the paternity
#'  index, \eqn{PI = T(O|M,F) / T(O|M)}, for that putative father.  An
#'  offspring allele missing from \code{freqs} has frequency zero.
#'
#' With one parent and no \code{freqs}, the value is a compatibility score:
#'  the probability of the offspring were the parent crossed with a mate of
#'  the offspring's own genotype (the parent's chance of transmitting a shared
#'  allele, halved for a heterozygous offspring).  It is zero when the parent
#'  is excluded and can rank candidate parents (as \code{parent_finder()}
#'  does), but it is not a likelihood and should not be used as the
#'  denominator of a paternity index.
#' @param off A locus from the offspring
#' @param mom A locus from one parent
#' @param dad A locus from another parent
#' @param freqs An optional allele frequency table, as returned by
#'  \code{frequencies()} (columns Allele and Frequency, plus Locus for
#'  multilocus data), used only when \code{dad} is missing.  For a single
#'  locus it must hold one locus.
#' @param multilocus A flag indicating that a single multilocus estimate
#'  of the transition probability should be passed (the default action).  
#'  If \code{FALSE}, the results will be returned as a vector of individual
#'  locus values.
#' @return The numeric probability of the offspring given the parents
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' off <- locus( 1:2 )
#' mom <- locus( c(1,1) )
#' dad1 <- locus( c(2,2) )
#' dad2 <- locus( c(1,2) )
#' dad3 <- locus( c(1,1) )
#' transition_probability( off, mom, dad1 )
#' transition_probability( off, mom, dad2 )
#' transition_probability( off, mom, dad3 )
#'
#' # one parent and a random mate with these allele frequencies
#' freqs <- data.frame( Allele = c("1","2"), Frequency = c(0.8, 0.2) )
#' transition_probability( off, mom, freqs = freqs )
transition_probability <- function( off, mom, dad , freqs, multilocus=TRUE){
  
  if( class(off) != class(mom) )
    stop("You need to pass either a set of 'locus' or 'data.frame' objects to this function.")
  
  ret <- NA
  
  # multilocus types
  if( is(off,'data.frame') ){
    loci <- column_class(off,'locus' )
    ret <- list()  
    
    for( locus in loci ) {
      oloc <- off[1,][[locus]]
      mloc <- mom[1,][[locus]]
      if( missing(dad) && !missing(freqs) ) {
        if( !("Locus" %in% names(freqs)) )
          stop("For multilocus data, 'freqs' needs a Locus column, as from frequencies().")
        ret[[locus]] <- transition_probability( oloc, mloc, 
                                                freqs = freqs[ freqs$Locus == locus, ] )
      }
      else if( missing(dad) )
        ret[[locus]] <- transition_probability( oloc,mloc )
      else
        ret[[locus]] <- transition_probability( oloc,mloc,dad[1,][[locus]])            
    }
    
    ret <- unlist( ret )    
    if( multilocus ) {
      ret <- ret[ !is.na(ret) ]
      ret <- as.numeric( cumprod( ret ))[length(ret)]
    }
  }
  
  # single locus estimates
  else if( is( off, "locus") ) {  
    
    # not missing
    if( !is.na(mom) & !is.na(off) ) {
      
      # one parent and a random mate
      if( missing(dad) && !missing(freqs) ){
        if( "Locus" %in% names(freqs) && length(unique(freqs$Locus)) > 1 )
          stop("For a single locus, 'freqs' must hold only that locus.")
        o <- alleles( off )
        if( length(o) != 2 )
          stop("Transition probabilities with 'freqs' are only defined for diploid loci.")
        m <- alleles( mom )
        p <- function( a ) sum( freqs$Frequency[ as.character(freqs$Allele) == a ] )
        ret <- 0
        for( a in m ) {
          if( a == o[1] )
            ret <- ret + p( o[2] ) / length(m)
          else if( a == o[2] )
            ret <- ret + p( o[1] ) / length(m)
        }
      }
      
      # one parent, compatibility only
      else if( missing(dad) ){
        ret <- ifelse( is_heterozygote(mom) , 0.5, 1.0 )
        if( is_heterozygote(off) ) 
          ret <- ret/2
        ret <- ret * length(intersect( alleles(mom), alleles(off) ) )
      }
      
      # two parent
      else {
        if( !is.na(dad)) {
          m <- alleles( mom )
          d <- alleles( dad )
          
          offs <- locus( cbind( rep(m, times=length(d) ), rep(d,each=length(m))), type="column", phased=FALSE )
          off <- locus( alleles(off), phased=FALSE )
          
          ret <- sum( off==offs ) / length(offs)                 
        }
      }
    }
  }
  return( ret )  
  
}
