#' Translates a \code{data.frame} with loci into a textual STRUCTURE file.
#' 
#' This is a basic converter that takes a \code{data.frame} with \code{locus} 
#'  objects in it and returns a textual representation as a STRUCTURE input
#'  file.
#' @param df The \code{data.frame} to be written to the output.
#' @param stratum The stratum to use as "POP" (default="Population")
#' @return A string representation of the \code{data.frame} formatted for STRUCTURE
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' A <- locus( c("1","1"))
#' B <- locus( c("1","2"))
#' C <- locus( c("2","2"))
#' loc1 <- c( A, A, B, B, B, B, C, C)
#' loc2 <- c( A, B, B, C, A, B, A, C)
#' pop <- data.frame( Population=c( rep("A",4),rep("B",4) ), loc1, loc2)
#' st <- to_structure( pop )
#' cat(st)
to_structure <- function( df, stratum="Population") {
  
  if( !inherits(df,"data.frame"))
    stop("This function is designed to save data frames to file")
  if( length(column_class(df,"locus")) < 1 )
    stop("There are no loci in this data.frame...  How do you want me to translate it into a genepop file?")
  if( is.null(stratum) || !(stratum %in% names(df)))
    stop("You need to specify the correct stratum to make a genepop file.")

  ret <- character(2*nrow(df))
  locus_names <- column_class(df,"locus")
  ctr <- 1
  
  pop <- as.character(as.numeric(factor(df[[stratum]])))
  
  for( i in 1:nrow(df)){
    t <- character(length(locus_names))
    b <- character(length(locus_names))
    
    for( j in 1:length(locus_names)){
      a <- alleles( df[[locus_names[j]]][i] )
      if( length(a)==2 ){
        t[j] <- as.character(a[1])
        b[j] <- as.character(a[2])
      }
      else 
        t[j] <- b[j] <- "-9"    
    }
    t <- c(as.character(i), pop[i], t)
    b <- c(as.character(i), pop[i], b)
    
    ret[ctr] <- paste(t,collapse=" ")
    ret[ctr+1] <- paste(b,collapse=" ")
    ctr <- ctr + 2
  }
   
  return( paste(ret,collapse="\n" ))
}