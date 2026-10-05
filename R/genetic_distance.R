#' Estimate genetic distances among individuals or strata
#' 
#' This is the primary front-end function for estimating genetic distances among
#'  either individuals or strata.
#' @param x A \code{data.frame} object with \code{\link{locus}} columns, or a single \code{locus} vector.
#' @param stratum The strata by which genetic distances are estimated (default="Population").
#'    Optional for individual-based distance metrics like AMOVA and Bray.
#' @param mode The genetic distance metric to use:
#'    \describe{
#'      \item{amova}{Inter-individual squared Euclidean distance (Peakall et al. 1995, Smouse & Peakall 1999)}
#'      \item{bray}{Inter-individual Bray-Curtis / shared allele distance (Bowcock et al. 1994)}
#'      \item{dps}{Stratum-level shared allele distance (1 - Ps)}
#'      \item{cavalli}{Cavalli-Sforza & Edwards (1967) chord distance}
#'      \item{cgd}{Conditional Genetic Distance via Population Graphs (Dyer & Nason 2004)}
#'      \item{pgd}{Partitioned (proportional) Conditional Genetic Distance: each
#'        Population Graph edge's cGD split into two directed shares in proportion to
#'        the genetic-gravity neighbourhood weights (see \code{\link{pgd}} and
#'        \code{vignette("genetic_gravity")}), returned as all-pairs directed
#'        shortest-path distances.  The matrix is \strong{asymmetric}: \code{[i, j]}
#'        is the distance from \code{i} to \code{j}, shorter in the direction of
#'        gene flow.  The bandwidth is set with \code{gamma} (default \code{0.5}).}
#'      \item{euclidean}{Euclidean allele frequency distance}
#'      \item{jaccard}{Jaccard set dissimilarity}
#'      \item{nei}{Nei's unbiased genetic distance (1978)}
#'      \item{ss}{Partitioned Sum of Squares distance from AMOVA}
#'    }
#' @param ... Options for the chosen metric.  Currently only \code{gamma}, the
#'    bandwidth multiplier for \code{mode = "pgd"}; other modes take none, and
#'    unused arguments are reported.
#' @return A pairwise distance matrix (asymmetric for \code{mode = "pgd"}).
#' @seealso \code{\link{pgd}} to compute pGD from an existing Population Graph.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' AA <- locus( c("A","A") )
#' AB <- locus( c("A","B") )
#' BB <- locus( c("B","B") )
#' loci <- c(AA,AB,AB,BB,BB,AA,AB,BB,AA,BB) 
#' df <- data.frame( Population=c(rep("Pop-A",5), rep("Pop-B",5)), TPI=loci)
#' genetic_distance(df, mode="amova")
#' genetic_distance(df, mode="dps")
#' genetic_distance(df, mode="cavalli")
#' genetic_distance(df, mode="nei")
#' \donttest{
#' data(gravity)
#' P <- genetic_distance(gravity, mode = "pgd")
#' P[1:4, 1:4]   # rows: from, columns: to
#' }
genetic_distance <- function( x, stratum="Population", mode, ... ){

  if( missing(mode)) 
    stop("You need to indicate which genetic distance you would like to use.")
  
  mode <- tolower(mode)

  if( is(x,"locus") ) {
    x <- data.frame(Locus=x,Stratum=stratum)
    stratum <- "Stratum"
  }
  
  if( !is(x,"data.frame") )
    stop("You must either pass a 'locus' vector or a 'data.frame' with 'locus' objects in it to this function.")

  stratum <- .detect_stratum(x, stratum, default = "Population", explicit = !missing(stratum))
  x <- .plain_df(x)

  ret <- NULL
  if( mode != "pgd" )
    chkDots(...)

  if( mode == "amova" )
    ret <- dist_amova(x)
  
  else if( mode == "bray"){
    x[[stratum]] <- 1:length(x[,1])
    ret <- dist_bray(x=x,stratum=stratum)
  }
  
  else if( mode == "euclidean") 
    ret <- dist_euclidean(x,stratum=stratum)
  
  else if( mode == "cgd") 
    ret <- dist_cgd(x,stratum=stratum)
  
  else if( mode == "pgd")
    ret <- dist_pgd(x, stratum=stratum, ...)
  
  else if( mode == "nei")
    ret <- dist_nei(x,stratum=stratum)
  
  else if( mode == "dps")
    ret <- dist_bray(x,stratum=stratum)
  
  else if( mode == "jaccard" )
    ret <- dist_jaccard(x,stratum=stratum)
  
  else if( mode %in% c("cavalli", "chord") )
    ret <- dist_cavalli(x, stratum=stratum)
  
  else if( mode == "ss" )
    ret <- dist_ss(x, stratum=stratum)
  
  else
    stop("Unrecognized genetic distance metric being requested.")


  return(ret)
}




