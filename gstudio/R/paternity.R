#' Estimates fractional paternity probability
#'
#' This function estimates fractional paternity for offspring
#'  given a set of potential fathers.
#' @param offspring A particular offspring. Often as a row
#'  from a \code{data.frame} with columns as loci and other 
#'  meta data.
#' @param mother The assumed mother of the offspring as a row
#'  from a \code{data.frame} with columns as loci and other 
#'  meta data.
#' @param fathers A \code{data.frame} of potential fathers.
#' @param ID The name of the column where the dad's adult ID column
#'  is found in the \code{data.frame} of potential fathers.
#' @param OffID The name of the column where the offspring ID is 
#'  located.  All offspring from a maternal individual should have
#'  have the same maternal \code{ID} to indicate which mother they
#'  are from but must also have a unique offspring ID.
#' @return A \code{data.frame} with indications of paternity by row.  Columns 
#'  will include ID, OffID, DadID, and potentially Fij.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' freqs <- c(0.55, 0.30, 0.15, 0.34, 0.34, 0.32)
#' loci <- c(rep("TPI",3), rep("PGM",3))
#' alleles <- c(LETTERS[1:3],LETTERS[8:10])
#' f <- data.frame(Locus=loci, Allele=alleles, Frequency=freqs)
#' adults <- make_population(f,N=20)
#' adults
#' offs <- mate( adults[1,], adults[2,], N=10)
#' offs
#' paternity( offs, adults[1,], adults )
paternity <- function( offspring, mother, fathers, ID="ID", OffID="OffID"){

  if( missing(offspring) | missing(mother) | missing(fathers) )
    stop("you need to pass offspring, mother, and putative fathers to paternity()")
  
  if( !(ID %in% names(offspring)) | !(ID %in% names(mother)) | !(ID %in% names(fathers)))
    stop("You need to have an ID column in offspring, mother, and putative father data sets.")
  
  
  
  locus_names <- column_class(offspring,"locus")
  if( !(all( locus_names == column_class(mother,"locus"))))
    stop("You need to have the same loci in both mother and offspring for paternity() to work.")
  if( !(all( locus_names == column_class(fathers,"locus"))))
    stop("You need to have the same loci in both potential dads and offspring for paternity() to work.")
  
  
  K <- dim(offspring)[1]
  N <- length(fathers[[ID]])
  ret <- data.frame()
  
  # TODO iterate across offspring
  for( off in 1:K) {
    
    oret <- data.frame(MomID=mother[[ID]], OffID=offspring[off,][[OffID]], DadID=fathers[[ID]],  Fij=0)
    
    for( i in 1:N) {
      fij <- NA
      
      for( locus in locus_names){
        o <- offspring[off,][[locus]]
        m <-mother[[locus]]
        f <- fathers[i,][[locus]]
        
        if( !is.na(o) & !is.na(m) & !is.na(f) ) {
          r <- transition_probability(o,m,f)
          if( !is.na(fij) )
            fij <- fij * r
          else if( is.na(fij) & r>0)
            fij <- r
        }
      }
      oret$Fij[i] <- fij
    }
    
    oret <- oret[ !is.na(oret$Fij),]
    oret$Fij <- oret$Fij/ sum(oret$Fij, na.rm=TRUE)
    oret <- oret[ oret$Fij>0 , ]
    ret <- rbind( ret, oret )
  }
  
  rownames(ret) <- 1:length(rownames(ret))
  
  return(ret)
}

