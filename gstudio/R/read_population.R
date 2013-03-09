#' Read a raw text file in and translate appropriate columns into genotypes
#' 
#' The function reads in a text file and does the proper translations for 
#'  genotypes and spatial coordinates.  
#' @param path The path to the text file
#' @param sep The field separator
#' @param header A logical flag indicating if there is a header row (a good
#'    idea).  
#' @param locus.columns A vector indicating the numerical column number for 
#'    each data type that will be treated as a \code{locus} object.
#' @param is.two.column A logical If false, genotypes will not be read in as two columns
#'    of data.  If you do have genotypes in two-column format then pass to this
#'    parameter an array indicating the column numbers at which the first column
#'    of the data start.  For example, if you have a column of , strata 
#'    and three two-column loci (your file has 7 columns of data) you would pass 
#'    two.colum=c(2,4,6).
#' @param is.snp.minor A flag sent to \code{locus} to indicate that the encoding is
#'    as the number of minor SNP's in the genotype.  This is a one column format.
#' @param is.zyme A flag sent to \code{locus} indicating that the alleles are in
#'    a format similar to allozyme loci.  This is a one-column format
#' @param is.phased A flag sent to \code{locus} indicating that the order of the
#'    alleles is important (e.g., they have been phased).
#' @param is.separated A flag indicating that the genotypes are 1-column and are
#'    encoded as a character string with alleles separated by a colon ':'
#' @param is.aflp A flag indicating that the columns of loci are dominant AFLP loci
#' @return A \code{data.frame} with \code{locus} columns pre-formatted.
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
#' @examples
#' \dontrun{
#' # an example for reading in a data set with tab separation and alleles encoded 
#' # in two columns each.  These start at column 4 and go to 24 (e.g., 5 loci)
#' data.2col <- read_population( "./myfile.txt", header=T, sep="\t", two.column=TRUE, locus.columns=4:24)
#' 
#' # an example of an aflp data set with 220 loci encoded as 0/1 in a comma separated data file.
#' data.aflp <- read_population( "./aflps.csv, header=T, sep=",", is.aflp=TRUE, locus.columns=2:221 )
#' }
#'    

read_population <- function( path, sep=",", header=TRUE, 
                             locus.columns=NULL, 
                             is.two.column=FALSE,
                             is.snp.minor=FALSE,
                             is.zyme=FALSE,
                             is.separated=FALSE,
                             is.phased=FALSE,
                             is.aflp=FALSE) {
  
  # read in the raw , object
  df <- read.table(path, sep=sep,header=header,stringsAsFactors=FALSE)
  if( !is(df,"data.frame"))
    stop("Unable to create a data frame from that file.")
  
  if( ncol(df)<2 )
    warning("Your data frame does not have many columns of data in it.  This is a problem.  Check your 'sep' on the call.")
  
  # catch bad locus.columns parameter
  if( is.null(locus.columns) ) {
    warning("You need to specify which columns are intended to be loci in this file.")
    return(NULL)
  }
  
  # check the range of the locus.columns
  if( !is(locus.columns, "numeric"))
    stop("Invalid value passed as 'locus.co.umns'")
  
  if( min(locus.columns ) < 1 )
    stop("Error, the minimum value for locus.columns must be strictly greater than zero.")
  
  if( max(locus.columns) > (dim(df)[2]) ) 
    stop(paste("Your data file has fewer columns than you expect (has ", dim(df)[2], " expected at least ",max(locus.columns), ")", sep=""))

    
  
  # make return object that is all the non-genetic stuff to the left of the genotypes
  end.col <- min(locus.columns)
  if( is.two.column )
    end.col <- end.col - 1 
  
  if( end.col < 1) {
    ret <- data.frame(ID=1:dim(df)[1])
  }
  else {
    ret <- as.data.frame( df[,1:(end.col-1),] )
    names(ret) <- names(df)[1:(end.col-1)]
  }
  
  # read in the columns and 22
  for( locCol in locus.columns ){
    alleles <- df[,locCol]
          
    if( is.two.column )
      alleles <- df[,locCol:(locCol+1)]
    if( is.aflp )
      tmp <- locus( data.frame(alleles), is.snp.minor=is.snp.minor, is.zyme=is.zyme, is.phased=is.phased, is.separated=is.separated ) 
    else
      tmp <- locus( alleles, is.snp.minor=is.snp.minor, is.zyme=is.zyme, is.phased=is.phased, is.separated=is.separated ) 
    locus_name <- names(df)[locCol]
    ret[[locus_name]] <- tmp  
  }
  
  loci <- column_class(ret,"locus")
  if( any(is.na(column_class(ret,"locus")) ))
    warning("There were no Loci configured with these data, you may want to look at the parameters passed.")
  
  return(ret)
}



