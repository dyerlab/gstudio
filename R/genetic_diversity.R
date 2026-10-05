#' Estimate genetic diversity among strata in a Population,
#' 
#' This function is the main one used for estimating genetic diversity among 
#'  strata.  Given the large number of genetic diversity metrics, not all 
#'  potential types are included.  
#' @param x A \code{data.frame} object with \code{\link{locus}} columns.
#' @param stratum The name of the column holding strata.  If given, each
#'    statistic is estimated within each stratum (the \code{Hes}, \code{Hos}
#'    and \code{Ht} modes require it and pool across strata instead).  If not
#'    given, it is taken from \code{dplyr::group_by()} when \code{x} is grouped;
#'    otherwise all individuals are treated as one sample.
#' @param loci The set of loci to use (default NULL will use all)
#' @param mode The genetic diversity metric(s) to estimate.  This is
#'    the only interface to these estimators; there are no standalone
#'    \code{A()}, \code{He()}, \code{Fis()}, ... functions.  Pass several
#'    (e.g. \code{c("A","Ae","He")}) to get one table with a column for each,
#'    joined by \code{Stratum} and \code{Locus}.  \code{Hi} cannot be combined
#'    with other modes and, with a stratum, the pooled modes (\code{Hes},
#'    \code{Hos}, \code{Ht}) can only be combined with each other.  Options:
#'    \describe{
#'      \item{A}{Number of alleles}
#'      \item{Ae}{Effective number of alleles (default)}
#'      \item{A95}{Number of alleles with frequency at least five percent}
#'      \item{He}{Expected heterozygosity}
#'      \item{Ho}{Observed heterozygosity}
#'      \item{Hi}{Individual heterozygosity, the fraction of non-missing loci
#'        that are heterozygous in each individual.  Returns one row per
#'        individual with the non-locus columns of \code{x} and \code{stratum}
#'        is ignored.}
#'      \item{Hes}{Expected subpopulation heterozygosity, Nei's unbiased Hs
#'        (requires \code{stratum}).}
#'      \item{Hos}{Observed heterozygosity averaged across strata after Nei
#'        (requires \code{stratum}).}
#'      \item{Ht}{Nei's unbiased total expected heterozygosity (requires
#'        \code{stratum}).}
#'      \item{Fis}{Wright's inbreeding coefficient, 1 - Ho/He.}
#'      \item{Pe}{Locus polymorphic index, sum(p(1-p)).}
#'    }
#' @param small.N Apply the 2N/(2N-1) small sample size correction to the
#'    expected heterozygosity (modes \code{He}, \code{Fis}).
#' @param ... Other parameters
#' @return A \code{data.frame} with columns \code{Locus} and one named for the
#'    mode, plus \code{Stratum} if estimated within strata.  \code{Hes} and
#'    \code{Hos} add a \code{Multilocus} row when several loci are used.
#'    \code{Hi} returns one row per individual (see above).  With several
#'    modes there is one column per mode, in the order requested; rows present
#'    for only some modes (e.g. the \code{Multilocus} row) are \code{NA} for
#'    the others.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#'  AA <- locus( c("A","A") )
#'  AB <- locus( c("A","B") )
#'  BB <- locus( c("B","B") )
#'  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
#'  locus2 <- c(AB,BB,AA,BB,BB,AB,AB,AA,AA,BB)
#'  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  genetic_diversity( df, mode="Ae")
#'  genetic_diversity( df, mode=c("A","Ae","He","Ho") )
#'  genetic_diversity( df, stratum="Population", mode="He")
#'  genetic_diversity( df, stratum="Population", mode="Hes")
#'  genetic_diversity( df, mode="Hi")
#'  genetic_diversity( locus, mode="Fis")
genetic_diversity <- function( x, stratum=NULL, loci=NULL, mode=c("A","Ae","A95","He", "Ho", "Hi", "Hos", "Hes", "Ht", "Fis","Pe")[2] , small.N=FALSE, ... ){
  rmode <- mode
  mode <- tolower(mode)
  
  if( is(x,"locus"))
    x <- data.frame(Locus=x)
  if( !is(x,"data.frame") || length( column_class(x,"locus")) < 1 )
    stop("Either pass a data.frame or an object of type locus to this function.")
  if( length(mode) < 1 || !all(mode %in% c("a","ae","a95","he", "ho", "hi", "hos", "hes", "ht", "fis","pe")))
    stop("Unrecognized mode passed to genetic_diversity().")
  
  stratum <- .detect_stratum(x, stratum, default = NULL, explicit = !missing(stratum))
  x <- .plain_df(x)
  
  # Restrict to the requested loci (keeping all non-locus columns)
  all_loci <- column_class(x,"locus")
  if( is.null(loci) )
    loci <- all_loci
  if( !all( loci %in% all_loci ))
    stop("You are asking for loci not present in the data.frame passed to genetic_diversity().")
  x <- x[ , c( setdiff(names(x), all_loci), loci ), drop=FALSE ]
  
  # Several modes: estimate each and join them into one table
  if( length(mode) > 1 ) {
    rmode <- rmode[ !duplicated(mode) ]
    mode <- unique(mode)
    if( "hi" %in% mode )
      stop("Hi is estimated per individual and cannot be combined with other modes.")
    pooled <- mode %in% c("hes","hos","ht")
    if( !is.null(stratum) && any(pooled) && !all(pooled) )
      stop("With a stratum, the pooled modes (Hes, Hos, Ht) return one row per locus and cannot be combined with per-stratum modes.")
    res <- lapply( rmode, function(m) genetic_diversity( x, stratum=stratum, loci=loci, mode=m, small.N=small.N, ... ) )
    ret <- Reduce( function(a, b) dplyr::full_join( a, b, by=intersect( c("Stratum","Locus"), names(a) ) ), res )
    rownames(ret) <- NULL
    return( ret )
  }
  
  # Individual heterozygosity is per row, not per stratum
  if( mode == "hi" ) {
    ret <- Hi(x)
    names(ret)[ncol(ret)] <- rmode
    return( ret )
  }
  
  if( mode %in% c("hes","hos","ht") && is.null(stratum) )
    stop("The Hes, Hos, and Ht modes require you to specify a stratum.")
  
  ## Passed with stratum 
  if( !is.null(stratum) ) {
    
    if( !(stratum %in% names(x)))
      stop("Requested stratum is NOT in the data you passed to genetic_diversity()...")
    
    # Asking for Nei's corrected stratum estimates
    if( mode %in% c("hos","hes","ht")) {
      if( mode == "hes")
        ret <- Hes(x,stratum=stratum)
      else if( mode == "hos" )
        ret <- Hos(x,stratum=stratum) 
      else
        ret <- Ht(x,stratum=stratum)
    }
    
    # Not summarized across stratum
    else {
      
      pops <- partition( x, stratum=stratum )
      ret <- data.frame(Stratum=NA, Locus=NA, Value=NA )
      
      for( pop in names(pops)){
        gd <- genetic_diversity( pops[[pop]], mode=mode, small.N=small.N, ...)
        gd$Stratum <- pop
        gd <- gd[,c(3,1,2)]
        if( names(ret)[3] == "Value" )
          names(ret)[3] <- names(gd)[3]
        ret <- rbind( ret, gd )
      }
      ret <- ret[ !is.na(ret$Stratum),]
      
    }
  }
  
  ## Passed without Stratum
  else { 
    ret <- data.frame( Locus=NA, Value=NA )
    
    
    for( locus in loci ){
      if( mode == "a")
        val <- A(x[[locus]], ...)
      else if( mode == "ae")
        val <- Ae(x[[locus]], ...)
      else if( mode == "a95")
        val <- A(x[[locus]],min_freq=0.05, ...)
      else if( mode == "he")
        val <- He(x[[locus]], small.N=small.N, ...)
      else if( mode == "ho")
        val <- Ho(x[[locus]], ...)
      else if( mode == "fis")
        val <- Fis(x[[locus]], stratum=stratum, small.N = small.N, ...)
      else if( mode == "pe")
        val <- Pe(x[[locus]], ...)
      else
        stop(paste("The type of diversity measure '", mode, "' you requested was not recognized.", sep=""))

      ret <- rbind( ret, data.frame(Locus=locus, Value=val ) )

    }
    
    names(ret)[2] <- mode
    ret <- ret[ !is.na(ret$Locus), ]
    rownames(ret) <- seq(1,nrow(ret))
  }
    
  names(ret)[ncol(ret)] <- rmode
  
  return( ret )     
  
  
}
