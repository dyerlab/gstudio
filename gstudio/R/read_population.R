#' Read a raw text file in and translate appropriate columns into genotypes
#' 
#' The function reads in a text file and does the proper translations for 
#'  genotypes and spatial coordinates.  
#' @param path The path to the text file
#' @param type An indication of what kind of loci that the data represent. The 
#'  following kinds are recoginzed (n.b., if you have several types load them
#'  separately and \code{merge} them).
#' \itemize{
#'  \item{missing}{The default. This will cause \code{read.population()} to read each column as a single locus with one allele}
#'  \item{aflp}{Encoded as 0,1 for absence/presence of bands.}
#'  \item{column}{Two columns of alleles per locus.}
#'  \item{separated}{Pre-separated alleles (with ':').}
#'  \item{snp}{Encoded by the number of minor alleles at the locus.}
#'  \item{zyme}{Alleles like zymes (e.g., 12 for '1' and '2' alleles).}
#' }
#' @param phased A flag indicating the the alleles should are of
#'  known gametic phase (default=FALSE).
#' @param sep The field separator
#' @param header A logical flag indicating if there is a header row (a good
#'    idea).  
#' @param locus.columns A vector indicating the numerical column number for 
#'    each data type that will be treated as a \code{locus} object.
#' @return A \code{data.frame} with \code{locus} columns pre-formatted.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
read_population <- function( path, type, locus.columns, phased=FALSE, sep=",", header=TRUE ) {
  
  # Catch obvious errors
  if( is.null(locus.columns) ) 
    stop("You need to specify which columns are intended to be loci in this file.")
  if( !is(locus.columns, "numeric"))
    stop("Invalid value passed as 'locus.columns'")  
  

  df <- read.table(path, sep=sep,header=header,stringsAsFactors=FALSE)
  
  if( ncol(df)==1 )
    warning("Your data.frame has only 1 column, did you misspecify the 'sep' character?")    
  if( max(locus.columns) > (dim(df)[2]) ) 
    stop(paste("Your data file has fewer columns than you expect (has ", dim(df)[2], " expected at least ",max(locus.columns), ")", sep=""))
    
  
  # make return object that is all the non-genetic stuff to the left of the genotypes
  end.col.meta.data <- min(locus.columns) - 1
  if( end.col.meta.data == 0 ) {
    ret <- data.frame(ID=seq(1, length(df[,1])))
  }
  else {
    ret <- df[,1:end.col.meta.data]
  }
    
  # drop even numbered columns if full sequence given
  if( type=="column" && all(diff(locus.columns)==1) )
    locus.columns <- seq(min(locus.columns), (max(locus.columns)-1), by=2)
  
  
  # read them in column-wise
  for( locCol in locus.columns ){
    if( type=="column")
      alleles <- df[,locCol:(locCol+1)]
    else
      alleles <- df[,locCol]
    tmp <- locus( alleles, type=type, phased=phased)  
    locus_name <- names(df)[locCol]
    ret[[locus_name]] <- tmp  
  }
  
  
  loci <- column_class(ret,"locus")
  if( any(is.na(column_class(ret,"locus")) ))
    warning("There were no Loci configured with these data, you may want to look at the parameters passed.")
  
  return(ret)
}



