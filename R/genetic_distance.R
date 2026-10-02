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
#'      \item{euclidean}{Euclidean allele frequency distance}
#'      \item{jaccard}{Jaccard set dissimilarity}
#'      \item{nei}{Nei's unbiased genetic distance (1978)}
#'      \item{ss}{Partitioned Sum of Squares distance from AMOVA}
#'    }
#' @return A pairwise distance matrix.
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
genetic_distance <- function( x, stratum="Population", mode ){

  if( missing(mode)) 
    stop("You need to indicate which genetic distance you would like to use.")
  
  mode <- tolower(mode)

  if( is(x,"locus") ) {
    x <- data.frame(Locus=x,Stratum=stratum)
    stratum <- "Stratum"
  }
  
  if( !is(x,"data.frame") )
    stop("You must either pass a 'locus' vector or a 'data.frame' with 'locus' objects in it to this function.")

  ret <- NULL

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




