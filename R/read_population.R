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
#'  \item{haploid}{One column per locus.}
#'  \item{snp}{Encoded by the number of minor alleles at the locus.}
#'  \item{zyme}{Alleles like zymes (e.g., 12 for '1' and '2' alleles).}
#'  \item{genepop}{Import data that is in 'genepop' format.}
#'  \item{cdpop}{Import genotypes encoded by CDPOP for subsequent analyses.}
#'  \item{structure}{Import a STRUCTURE data file (2 rows per individual format).}
#' }
#' @param phased A flag indicating the the alleles should are of
#'  known gametic phase (default=FALSE).
#' @param sep The field separator for each column of the data.
#' @param delim The (optional) delimiter for alleles separators (default=":")
#' @param header A logical flag indicating if there is a header row (a good
#'    idea).  
#' @param locus.columns A vector indicating the numerical column number for 
#'    each data type that will be treated as a \code{locus} object.
#' @param ... Optional parameters you can pass to \code{read.csv} or \code{read.table}.
#' @return A \code{data.frame} with \code{locus} columns pre-formatted.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
read_population <- function( path, type, locus.columns, phased=FALSE, sep=",", header=TRUE, delim=":",...) {
  type <- tolower(type)
  
  # catch if path is url
  if( any( grepl("https://docs.google.com/spreadsheets/*", path)) {
    
  }
  
  
  if (!("textConnection" %in% class(file)))
  {
    if( !file.exists(path) ){
      ans <- paste("You did not pass a valid path to this function.  What you passed", 
                   path, 
                   "is not the FILE that has data in it, it does not exist." )
      stop(ans)    
    }
    
    if( file.info(path)$isdir ){
      stop("You passed a directory path, not a file path to read_population().  Pass a path to the actual FILE.")
    }
  }  
  if( !missing(type) && !(type %in% c("aflp","column","separated","snp","zyme","genepop","cdpop","haploid","structure")))
    stop("Unrecognized 'type' submitted to read_population().  Please specify which type of data file you are trying to load in.")
  
  # specify the haploid as separated, it will come out as a single column due to no separators
  if( type=="haploid"){
    type <- "separated"
  }
  
  
  # check for genepop and handle in its own 
  if( type == "genepop")
    return( .read_genepop(path) )
  
  # do the cdpop readin
  else if( type=="cdpop")
    return( .read_cdpop(path, ...) )
  
  else if( type=="structure")
    return( .read_structure( path, ...) )
  
  # default
  else 
    return( .read_columns( path, type, locus.columns, phased, sep, header, delim, ...))
  
}





# These are helper functions
.read_columns <- function( path, type, locus.columns, phased, sep, header, delim, ... ) {
  
  # Catch obvious errors
  if( is.null(locus.columns) ) 
    stop("You need to specify which columns are intended to be loci in this file.")
  if( !is(locus.columns, "numeric") )
    stop("Invalid value passed as 'locus.columns'")  
  
  df <- read.table(path, sep=sep,header=header, stringsAsFactors=FALSE, ...)
  
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
  
  ctr <- 0
  if( length(locus.columns) > 499 ) {
    cat("gstudio: Big Column Import [ 0")
  }
  
  # read them in column-wise
  for( locCol in locus.columns ){
    
    if( ctr > 0 ) {
      ctr <- ctr + 1       
      if( (ctr %% 500 ) == 0 ) {
        cat(" ",ctr)   
      }
    }
    
    if( type=="column") {
      alleles <- df[,locCol:(locCol+1)]
      tmp <- locus( alleles, type=type, phased=phased)  
    } else if( type=="separated")  {
      changeback <- TRUE
      genos <- df[,locCol]
      genos[ nchar(genos)== 0] <- NA
      genos[ is.na(genos) ] <- paste( c("NA","NA"), collapse=":")
      a <- strsplit(genos, split=delim, fixed=TRUE)
      alleles <- matrix( unlist(a), byrow = TRUE, ncol=2)
      tmp <- locus( alleles, type="column", phased=phased)  
    } else {
      alleles <- df[,locCol]
      tmp <- locus( alleles, type=type, phased=phased)  
    }
    
    locus_name <- names(df)[locCol]
    ret[[locus_name]] <- tmp  
  }
  
  if( ctr > 0 ) {
    cat(" ]\n")
  }
  
  
  loci <- column_class(ret,"locus")
  if( any(is.na(column_class(ret,"locus")) ))
    warning("There were no Loci configured with these data, you may want to look at the parameters passed.")
  
  return(ret)
}



.read_genepop <- function( path ){
  
  raw <- readLines( path, -1, skipNul = TRUE )
  raw <- stringi::stri_trans_general(raw, "latin-ascii")
  raw <- raw[ nchar(raw)>0 ]
  
  # remove trailing and leading whitespaces
  raw <- unlist(lapply( raw, function(x) gsub("^\\s+|\\s+$", "", x)))
  
  
  
  N <- length(raw)
  if( N<3)
    stop("Cannot load a genepop file with no data in it...")
  
  #find designations
  popidx <- which( raw %in% c("Pop","pop","POP", "POp", "pOP")) 
  if( length(popidx) < 1 )
    stop("You do not have 'Pop' designations in this file.")
  
  header <- raw[1:(popidx[1] - 1)]
  description <- header[1]
  locus_names <- strsplit(header[2:(length(header))], split=",")[[1]]
  K <- length(locus_names)
  if( K < 1 )
    stop("No loci?  What are you doing?")
  if( K == 1 ) {
    locus_names <- header[ 2:(length(header)) ]
    K <- length(locus_names)
  }
  
  
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
        raw_loci[ raw_loci == "0000" ] <- NA
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




.read_cdpop <- function( path, ... ) {
  data <- read.csv(path,stringsAsFactors=FALSE, ...)
  col_names <- names(data)
  locus_names <- grepl("L[[:digit:]]A[[:digit:]]+$",col_names)
  loci <- data[, col_names[ locus_names ]]
  df <- data[, col_names[ !locus_names] ]
  locus_names <- names(loci)
  
  
  # figure out how many loci are here
  last_loc <- names(loci)[ length(locus_names)]
  l <- strsplit(last_loc,split = "A")[[1]][1]
  l <- as.numeric(substr(l, 2, nchar(l)))
  for( i in 0:l){
    locus_name <- paste("Locus",i,sep="-")
    idx <- grep(paste("L",i,sep=""),locus_names)
    x <- loci[,idx]
    get_alleles <- function(x){
      a <- which(x!=0)
      a <- a-1
      if( length(a)==1)
        a <- c(a,a)
      return( paste(a,collapse=":"))
    }
    
    alleles <- locus(apply(x,1,get_alleles), type="separated")
    df[[locus_name]] <- alleles
  }
  
  return( df )
}




.read_structure <- function( path, ... ) {
  raw <- readLines(path)
  if( length(raw) < 2 )
    stop("There is a problem, you did not pass a valid path for the data file.")
  
  # clean up problems with mixed tabs and spaces, make all 
  .cleanup_and_split <- function( row ) {
    header <- gsub("\t",",",row )
    header <- gsub(" ", ",", header )
    columns <- strsplit(header,",",fixed=TRUE)[[1]]
    return(columns)
  }
  
  #convert to matrix  
  data <- matrix( unlist( lapply(raw, .cleanup_and_split )),nrow = length(raw) , byrow = TRUE)
  colnames(data) <- data[1,]
  df <- data.frame( data[2:nrow(data),])
  if( nrow(df) < 2 )
    stop("There is a problem, you did not pass a file that has more than 1 row in it...")
  
  
  # Make new
  loci <- names(df)[3:ncol(df)]
  data <- data.frame( ID=unique(df[,1]) )
  for(col in names(df)[3:ncol(df)]){
    data[[col]] <- rep(locus(),nrow(data))
  }
  
  # make
  for( i in seq(1,nrow(df),by=2) ) {
    id <- df$V1[i]
    row <- which( data$ID == id)
    for( l in loci ){
      alleles <- gsub("-9", NA, df[[l]][i:(i+1)])
      data[[l]][row] <- locus( alleles )
    }
  }
  
  ret <- merge( data, df[,1:2], by.x="ID", by.y="V1")
  ret <- ret[ , c(ncol(ret), 1:(ncol(ret)-1)) ]
  names(ret)[1] <- "Population"
  ret$Population <- factor( ret$Population )
  ret$ID <- factor( ret$ID )
  return( ret )
}





