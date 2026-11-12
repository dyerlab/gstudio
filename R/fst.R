#' Estimation Wright's Fst parameter
#' 
#' This function estimates Wright's Fst parameter, the one based upon the variance
#'  in p, most applicable to a 2-allele locus.  The results are returned
#'  as a \code{data.frame}.
#' @param x  A \code{data.frame} with \code{locus} objects.  If there is more than one 
#'  locus present, it will estimate the parameter for each locus and add a
#'  \code{Multilocus} row, 1 - mean(Hs)/mean(Ht) over the polymorphic loci (a ratio of
#'  averages, not an average of the single locus estimates).
#' @param stratum Either the name of the column representing strata in \code{x}.  By 
#'  default, this function will use "Population".
#' @param nperm The number of permutations to run to test Fst=0 hypothesis.
#' @return A \code{data.frame} with Locus, Fst, Hs and Ht (loci in column order),
#'  plus a \code{Multilocus} row when there is more than one locus.  When \code{nperm > 0} it also has \code{P},
#'  the right-tailed add-one permutation p-value for the null hypothesis Fst = 0
#'  (small P indicates significant structure).
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @noRd
#' @examples
#'  AA <- locus( c("A","A") )
#'  AB <- locus( c("A","B") )
#'  BB <- locus( c("B","B") )
#'  locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
#'  Population <- c(rep("Pop-A",5),rep("Pop-B",5))
#'  df <- data.frame(Population,locus)
#'  Fst( df )
#'  locus2 <- c(AA,AA,AA,AA,AA,BB,BB,BB,BB,BB)
#'  df <- data.frame( Population, TPI=locus, PGM=locus2 )
#'  Fst( df )
Fst <- function( x, stratum="Population", nperm=0  ) {
  
  if( !is(x,"data.frame"))
    stop("This function requires you to pass it a data.frame of data...")
  
  stratum <- .detect_stratum(x, stratum, default = "Population")
  x <- droplevels(x)

  loci <- column_class(x,"locus")
  if( any(is.na(loci) ) || ( !( any( loci %in% names(x)))))
    stop("You need to pass this function a data.frame with some columns of type 'locus' to it...")
    
  if( !(stratum %in% names(x)))
    stop("You need to pass this function the name of the column to use as a locus")
  
  hs <- Hes( x, stratum=stratum, do.multilocus=FALSE )
  ht <- Ht( x, stratum=stratum )
  hs_vec <- hs$Hes[ match( loci, hs$Locus ) ]
  ht_vec <- ht$Ht[ match( loci, ht$Locus ) ]

  # Monomorphic loci have Hs = Ht = 0 (up to rounding) and no defined Fst.
  tol <- sqrt( .Machine$double.eps )
  mono <- !is.na(ht_vec) & abs(ht_vec) < tol
  hs_vec[ mono ] <- 0
  ht_vec[ mono ] <- 0

  fst_of <- function( h_s ) {
    f <- 1 - h_s / ht_vec
    f[ mono ] <- NaN
    f
  }
  # Multilocus: ratio of mean Hs to mean Ht over the polymorphic loci.
  multi_of <- function( h_s ) {
    ok <- !mono & !is.na(h_s) & !is.na(ht_vec)
    if( !any(ok) ) return( NaN )
    1 - mean( h_s[ok] ) / mean( ht_vec[ok] )
  }

  ret <- data.frame( Locus = loci, Fst = fst_of( hs_vec ), Hs = hs_vec, Ht = ht_vec,
                     stringsAsFactors = FALSE )
  K <- length( loci )
  if( K > 1 ) {
    ret <- rbind( ret, data.frame( Locus = "Multilocus", Fst = multi_of( hs_vec ),
                                   Hs = mean( hs_vec, na.rm = TRUE ),
                                   Ht = mean( ht_vec, na.rm = TRUE ),
                                   stringsAsFactors = FALSE ) )
  }

  if( nperm > 0 ) {
    count <- numeric( nrow(ret) )
    used  <- numeric( nrow(ret) )
    tmp <- x
    message("permuting ", appendLF = FALSE)
    for( rep in seq(1,nperm) ) {

      if( rep%%10 == 0 ) {
        message(".", appendLF = FALSE)
      }
      # Label permutation (no replacement) for the H0: Fst = 0 null.
      tmp[[stratum]] <- sample( tmp[[stratum]] )
      suppressWarnings(
        hes_perm <- Hes( tmp, stratum=stratum, do.multilocus=FALSE )
      )
      hp <- hes_perm$Hes[ match( loci, hes_perm$Locus ) ]
      null <- fst_of( hp )
      if( K > 1 )
        null <- c( null, multi_of( hp ) )
      # Right tail: count permutations whose Fst meets/exceeds the observed.
      ok <- !is.na( null ) & !is.na( ret$Fst )
      count <- count + ( ok & null >= ret$Fst )
      used  <- used + ok
    }
    message("")
    # Add-one permutation p-value: (1 + #{perm >= obs}) / (1 + nperm), strictly
    # positive (Phipson & Smyth 2010, Stat. Appl. Genet. Mol. Biol. 9:Article39).
    ret$P <- ifelse( is.na( ret$Fst ), NA_real_, ( 1 + count ) / ( 1 + used ) )
  }

  return( ret )
}
