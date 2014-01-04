#' Translates a \code{data.frame}
#' 
#' This is a basic output file formatter for saving files necessary for saving
#'  output in textual formats.
#' @param df The \code{data.frame} to be written to the output.
#' @param stratum The stratum to use as "POP" (default="Population")
#' @return A string representation of the \code{data.frame} formatted as
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' A <- locus( c("1","1"))
#' B <- locus( c("1","2"))
#' C <- locus( c("2","2"))
#' loc1 <- c( A, A, B, B, B, B, C, C)
#' loc2 <- c( A, B, B, C, A, B, A, C)
#' pop <- data.frame( Population=c( rep("A",4),rep("B",4) ), loc1, loc2)
#' gp <- to_genepop( pop )
#' cat(gp)
to_genepop <- function( df, stratum="Population") {
  
  if( !inherits(df,"data.frame"))
    stop("This function is designed to save data frames to file")
  if( length(column_class(df,"locus")) < 1 )
    stop("There are no loci in this data.frame...  How do you want me to translate it into a genepop file?")
  if( is.null(stratum) || !(stratum %in% names(df)))
    stop("You need to specify the correct stratum to make a genepop file.")
  pops <- partition( df, stratum=stratum)
  N <- length(pops) + nrow(df) + 2
  loci <- column_class(df,"locus")
  
  digits <- 1
  for( locus in loci){
    nd <- max(nchar(alleles(df[[locus]],all=FALSE)))
    if( nd > digits ) 
      digits <- nd
  }
  
  
  # set up the stuff to return
  ret <- character(N)
  ret[1] <- "Data.frame exported from gstudio R package"
  ret[2] <- paste( paste(rep(" ",max(nchar(names(pops)))),collapse=""), paste( loci, collapse=", "), collapse="") 
  ctr <- 3
  
  for( popname in names(pops) ){
    ret[ ctr ] <- "POP"
    ctr <- ctr + 1
    
    pop <- pops[[popname]]
    for( i in 1:nrow(pop)){
      row <- paste(popname,",",sep="")
      l <- character(length(loci))
      for( j in 1:length(l))
        l[j] <- to_fixed_locus(pop[[loci[j]]][i], digits=digits)
      
      ret[ctr] <- paste(row,paste(l,collapse=" "),sep=" ")
      ctr <- ctr + 1
    }

  }
    
  
  return( paste(ret,collapse="\n" ))
}