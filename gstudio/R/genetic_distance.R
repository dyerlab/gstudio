#' Estimate genetic distances among strata in a ,
#' 
#' This function is the main one used for estimating genetic distances among 
#'  either individuals or stratum.  Given the large number of genetic distance
#'  metrics, some are recreated here, de novo, and some are estimated through 
#'  other existing R packages.  
#'  @param x A \code{data.frame} object with \code{\link{locus}} columns.
#'  @param stratum The strata by which the genetic distances are estimated.  This
#'    can be an optional parameter when estimating distance measures calculated
#'    among indiviudals (default='Population'). 
#'  @param mode The particular genetic distance metric that you are going to use. 
#'    The \code{gstudio} package currently includes the following individual distance 
#'    measures:
#'    \itemize{
#'      \item{AMOVA}{Inter-individual }
#'      \item{Bray}{Proportion of shared alleles}
#'      \item{Jaccard}{Jaccard set dissimilarity}
#'    }
#'  @param ... Ignored
#'  @return A \code{genetic_distance} object (also a matrix) with the genetic 
#'    distances and a bit of additional information about its creation.
#'  @export
#'  @author Rodney J. Dyer <rjdyer@@vcu.edu>
#'  
genetic_distance <- function( x, stratum="Population", mode=c("AMOVA","cGD")[1], ... ){
  
  
  if( !(mode %in% c("AMOVA","cGD","Jaccard","Bray","Euclidean","Nei","Dps", "cGD")))
    stop("Unrecognized genetic distance")
  
  # passing a df not pop
  if( inherits(x,"locus")) {
    data <- x
    N <- length(data)
    nLoc <- 1
  }
    
  else if( inherits(x,"data.frame")) {
    data <- x[,column_class(x,"locus",mode="index")]
    d <- dim(data)
    N <- d[1]
    nLoc <- d[2]
  }
    
  else
    stop("This function only works on objects of inheriting from 'data.frame' or 'locus")

  if( N < 2 )
    stop( "You need at least two entities to measure distance...")
  
  ret <- NULL
  idx <- indices(1:N)
  
  
  #######################  Individual Distances
  if( mode == "AMOVA" ) 
    ret <- apply( idx, 1, dist_amova, data=to_mv(data) ) 
  
  else if( mode=="Jaccard")
    ret <- apply( idx, 1, dist_jaccard, data=data, nLoc=nLoc)
  
  else if( mode == "BrayCurtis") 
    ret <- apply( idx, 1, dist_bray, data=as.matrix(data), nLoc=nLoc) 
  


  
   #######################   Graph Distance
   else if( mode == "cGD" ){
    warning("Not implemented yet.")      
   }
  
  
  #######################   Graph Distance
  else {
    data <- frequencies(x,stratum=stratum)
    attr(ret,"strata") <- row.names(data)
    
    if( mode=="Euclidean")
      ret <- apply( idx, 1, function(i,freqs) sqrt(sum( (freqs[i[1],]-freqs[i[2],])^2 )), freqs=data)
    
    # Cavalli-Sforza Edwards Distance
    else if( mode=="CavalliSforza")
      ret <- apply( idx, 1, dist_cavalli, data=data )

    # Nei Distance
    else if( mode=="Nei")  
      ret <- apply( idx, 1, dist.nei, data=data )
    
    else if( mode=="Dps")
      ret <- apply( idx, 1, dist_bray, data=data, nloc=nLoc )

    
  }

  m <- matrix(0,N,N)
  m[ lower.tri(m)] <- ret
  m <- m + t(m)
  
  return(m)
}




