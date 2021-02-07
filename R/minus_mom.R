#' Subtracts maternal component to offspring genotypes
#' 
#' This function removes the female component to the offspring
#'  genotypes. It is one step in the \code{2gener} analysis. The
#'  coding of the \code{MomCol} and the \code{OffCol} headers 
#'  are specific so that this algorithm can differentiate between
#'  maternal individuals and offspring. 
#' @param x A \code{data.frame} with mother and offspring \code{locus} columns.
#' @param MomCol The name of the column indicating maternal ID.  All offspring
#'  from a mother MUST have the same MomCol value.  
#' @param OffCol The name of the column indicating the offspring ID number.  It is
#'  required that maternal individuals have OffCol="0" indicating that this is the 
#'  mother of those offspring.
#' @param Check  A flag (default \code{FALSE}) that will run through the reduction
#'  process and produce a check of all mom/off/locus problems that are observed.
#' @return Either a \code{data.frame} object of only the offspring after removing the
#'  contribution of each maternal individual or a data.frame with 
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @export
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
#' EE <- locus( c("E","E") )
#' F  <- locus( c("F"))
#' G  <- locus( )
#' loci <- c(AA,AB,AC,AD,BB,BC,BD,CD,CD,DD) 
#' offID <- c(0 ,1, 2, 3, 0, 1, 2, 1, 0, 2 )
#' momID <- c(rep("A",4), rep("B",3), rep("C",3))
#' df <- data.frame( ID=factor(momID), OffID=factor(offID), TPI=loci )
#' minus_mom( df )
#' loci <- c(AA,AB,BB,AD,BB,BC,G,CD,EE,F) 
#' df <- data.frame( ID=factor(momID), OffID=factor(offID), TPI=loci )
#' minus_mom( df, Check=TRUE )
minus_mom <- function( x, MomCol="ID", OffCol="OffID", Check=FALSE  )  {
  
  
  # x is data.frame in 
  if( !is(x,"data.frame"))
    stop("You need to pass the data as a data.frame to the minus_mom() function.")
  else if( !(MomCol %in% names(x)))
    stop("You need to specify the column that represents the mother (adult) column for minus_mom() to work.")
  else if( !(OffCol %in% names(x)))
    stop("You need to specify the column that represents the offspring identification column for minus_mom() to work.")
  else {
    locus_names <- column_class(x,"locus")
    
    if( length( locus_names) == 0 )
      stop("You need to have some loci to use this function.")
    
    x[[OffCol]] <- as.character( x[[OffCol]] )
    moms <- x[ x[[OffCol]]=="0",]
    ret <- x[ x[[OffCol]]!="0",]
    N <- dim(ret)[1]
    
    if( Check ) {
      ret$Individual_OK <- TRUE
      ret$Cause <- ""
    }
    
    for( i in 1:N ){
      off <- ret[i,]
      momNum <- off[[MomCol]]
      mom <- moms[ moms[[MomCol]] == momNum , ]
      
      for( locus in locus_names ) {
        if( ploidy( off[[locus]]) == ploidy( mom[[locus]]  )) {
          
          loc <- off[[locus]] - mom[[locus]] 
          off[[locus]] <-  loc  
          
          if( Check ) {
            
            # unreduced 
            if( ploidy( loc ) > 1 ) {
              
              # homozygote
              if (!is_heterozygote( loc ) ) {
                off$Individual_OK[1] <- FALSE
                off$Cause[1] <- paste(off$Cause[1], "Unreduced Homozygote", locus)
              }
              # het and different from mom 
              else if( loc != mom[[locus]]) {
                off$Individual_OK[1] <- FALSE
                off$Cause[1] <- paste(off$Cause[1], "Bad Match", locus)
              }
            }
            
          }
          
        }
        ## Bad Ploidy and not because of missing genotype
        else if( !is.na( off[[locus]])  && !is.na( mom[[locus]]) ) {
          message(paste("Unable to subtract adult '",mom[[locus]],
                        "' from offspring '",off[[locus]],
                        "', result is unreduced.",sep=""))
          if( Check ) { 
            off$Individual_OK[1] <- FALSE
            off$Cause[1] <- paste( off$Cause[1], "Unequal Ploidy", locus) 
          }
        }
        
        ret[i,] <- off
      }
    }
    if( Check ) {
      ret <- ret[ !ret$Individual_OK,]
      ret$Individual_OK <- NULL
    } 
    return( ret )
  }
}




