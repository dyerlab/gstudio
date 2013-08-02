#' Estimate genetic distances among strata in a ,
#' 
#' This function is the main one used for estimating genetic distances among 
#'  either individuals or stratum.  Given the large number of genetic distance
#'  metrics, some are recreated here, de novo, and some are estimated through 
#'  other existing R packages.  
#' @param x A \code{data.frame} object with \code{\link{locus}} columns.
#' @param stratum The strata by which the genetic distances are estimated.  This
#'    can be an optional parameter when estimating distance measures calculated
#'    among indiviudals (default='Population'). 
#' @param mode The particular genetic distance metric that you are going to use. 
#' @return A \code{genetic_distance} object (also a matrix) with the genetic 
#'    distances and a bit of additional information about its creation.
#' @note This function currently includes the following individual distance 
#'    measures:
#'    \itemize{
#'      \item{AMOVA}{Inter-individual }
#'      \item{Bray}{Proportion of shared alleles}
#'    }
#'    This function also supports genetic distances based upon stratum distances.  The
#'    currently supported genetic distances are:
#'    \itemize{
#'      \item{Euclidean}{Euclidean frequency distance}
#'      \item{cGD}{Conditional Genetic Distance}
#'      \item{Nei}{Nei's corrected genetic distance (1978)}
#'      \item{Dps}{Shared allele distance = 1 - Ps}
#'      \item{Jaccard}{Jaccard set dissimilarity}
#'    }

#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' AA <- locus( c("A","A") )
#' AB <- locus( c("A","B") )
#' BB <- locus( c("B","B") )
#' AC <- locus( c("A","C") )
#' AD <- locus( c("A","D") )
#' BC <- locus( c("B","C") )
#' BD <- locus( c("B","D") )
#' CC <- locus( c("C","C") )
#' CD <- locus( c("C","D") )
#' DD <- locus( c("D","D") )
#' loci <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD) 
#' pops <- c(rep("A",5), rep("B",5))
#' df <- data.frame( Population=pops, TPI=loci)
#' genetic_distance(df, mode="AMOVA")
#' genetic_distance(df, mode="Dps")
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
  else {
    if( !(stratum %in% names(x) ) )
      stop("The stratum variable must be ")
  }
  
  ret <- NULL

  if( mode == "amova" )
    ret <- dist_amova(x)
  
  else if( mode == "bray"){
    x[[stratum]] <- 1:length(x[,1])
    ret <- dist_bray(x=x,stratum=stratum)
  }
  
  else if( mode == "euclidean") 
    ret <- dist_euclidean(x,stratum)
  
  else if( mode == "cgd") 
    ret <- dist_cgd(x,stratum)
  
  else if( mode == "nei")
    ret <- dist_nei(x,stratum)
  
  else if( mode == "dps")
    ret <- dist_bray(x,stratum)
  
  else if( mode == "jaccard" )
    ret <- dist_jaccard(x,stratum)
  
  else
    stop("Unrecognized genetic distance metric being requested.")


  return(ret)
}




