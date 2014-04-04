#' Read a raw text file in and translate appropriate columns into genotypes
#' 
#' The function reads in a text file and does the proper translations for 
#'  genotypes and spatial coordinates.  
#' @param path The path to the text file
#' @param type An indication of what kind of loci that the data represent. The 
#'  following kinds are recoginzed (n.b., if you have several types load them
#'  separately and \code{merge} them).
#' \describe{
#'  \item{missing}{The default. This will cause \code{read.population()} to read each column as a single locus with one allele}
#'  \item{aflp}{Encoded as 0,1 for absence/presence of bands.}
#'  \item{column}{Two columns of alleles per locus.}
#'  \item{separated}{Pre-separated alleles (with ':').}
#'  \item{snp}{Encoded by the number of minor alleles at the locus.}
#'  \item{zyme}{Alleles like zymes (e.g., 12 for '1' and '2' alleles).}
#'  \item{genepop}{Import data that is in 'genepop' format.}
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
read_population <- function( path, type, locus.columns, phased=FALSE, sep=",", header=TRUE) {
  if( !missing(type) && !(type %in% c("aflp","column","separated","snp","zyme","genepop")))
    stop("Unrecognized 'type' submitted to read_population()")
  # check for genepop and handle in its own 
  if( type == "genepop")
    return( .read_genepop(path) )
  else 
    return( .read_columns( path, type, locus.columns, phased, sep, header ))

}


# These are helper functions
.read_columns <- function( path, type, locus.columns, phased, sep, header ) {
  
  # Catch obvious errors
  if( is.null(locus.columns) ) 
    stop("You need to specify which columns are intended to be loci in this file.")
  if( !is(locus.columns, "numeric") )
    stop("Invalid value passed as 'locus.columns'")  
  
  df <- read.table(path, sep=sep,header=header, stringsAsFactors=FALSE)
  
  if( ncol(df)==1 )
    warning("Your data.frame has only 1 column, did you misspecify the 'sep' character?")    
  if( max(locus.columns) > (dim(df)[2]) ) 
    stop(paste("Your data file has fewer columns than you expect (has ", dim(df)[2], " expected at least ",max(locus.columns), ")", sep=""))
  
  
  # make return object that is all the non-genetic stuff to the left of the genotypes
  end.col.meta.data <- min(locus.columns) - 1
  if( end.col.meta.data == 0 ) {
    ret <- data.frame(ID=seq(1, length(df[,1])))
  }
  else if( end.col.meta.data == 1) {
    ret <- data.frame( df[,end.col.meta.data])
    names(ret)[1] <- names(df)[1]

  }
  else {
    ret <- df[,1:end.col.meta.data]
  }
  
  # drop even numbered columns if full sequence given
  if( type=="column" && all(diff(locus.columns)==1) )
    locus.columns <- seq(min(locus.columns), (max(locus.columns)-1), by=2 )
  
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



.read_genepop <- function( path ){
  
  raw <- readLines( path, -1 )
  raw <- unlist(lapply( raw, function(x) gsub("^\\s+|\\s+$", "", x)))
  
  
  
  N <- length(raw)
  if( length(raw)<3)
    stop("Cannot load a genepop file with no data in it...")
  
  #find designations
  popidx <- which( raw %in% c("Pop","pop","POP", "POp", "pOP")) 
  if( length(popidx) < 1 )
    stop("You do not have 'Pop' designations in this file.")
  
  header <- raw[1:(popidx[1] - 1)]
  description <- header[1]
  locus_names <- header[2:(length(header))]
  K <- length(locus_names)
  if( K < 1 )
    stop("No loci?  What are you doing?")
  
  ret <- data.frame(Population=rep(NA,N),ID=NA)
  for( locus in locus_names ){
    ret[[locus]] <- locus(NA) 
    class( ret[[locus]]) <- "locus"
  }
    
  
  
  # go through the remaining data and work it out.
  curPop <- 0
  for( i in popidx[1]:length(raw)){
    row <- gsub("\\t"," ", raw[i])
    line <- strsplit(row,split=",")[[1]]
    
    if( length(line)==1){
      curPop <- curPop + 1    
    }
    else if( length(line) > 1 ){
      
      id <- gsub("^\\s+|\\s+$", "", line[1]) 
      raw_loci <- strsplit(gsub("^\\s+|\\s+$", "", line[2]), " ")[[1]]
      raw_loci <- raw_loci[ nchar(raw_loci)>0]
      if( length(raw_loci) != length( locus_names) )
        message(paste("Individual at row",i,"does not have the right number of loci"))
      
      # diploid loci
      if( all( nchar(raw_loci) > 3 ) )  {
        raw_loci[ raw_loci == "000000"] <- NA
        loci <- unlist( lapply( raw_loci, function( x ) locus(x, type="zyme"))) 
      }
      else {
        raw_loci[ raw_loci=="000"] <- NA
        loci <- unlist( lapply( raw_loci, function( x ) locus(x))) 
      }
      
      if( length( loci )== length(locus_names) ) {
        pop <- paste( "Pop",curPop, sep="-" )
        ret$Population[i] <- pop
        ret$ID[i] <- id
        ret[i,3:(K+2)] <- loci
      }
    }
  }
  
  ret <- ret[ !is.na(ret$Population),]
  

  return( ret )
}

  
