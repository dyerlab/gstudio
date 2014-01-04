#' Writes data frame with genotypes and other data to a file
#' 
#' This is a basic output file formatter for saving files necessary for saving
#'  output in textual formats.
#' @param df The \code{data.frame} to be written to the output.
#' @param file The path to the file you want to write to.
#' @param mode Which format to use for writing data to file.  At present there
#'  are 
#' \describe{
#'  \item{text}{Outputs all data columns with loci treated as colon separated.  This is the default.}
#'  
#'  \item{genepop}{Saves genetic data into GENEPOP format. You must specify a stratum.}
#'  \item{structure}{Saves genetic data into the two-lined per individual STRUCTURE format.  You must specify a stratum.}
#' }
#' @param stratum An optional argument if using genepop or structure formats.  By 
#'  default, the 'text' option writes all data to file.
#' @return nothing.
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
write_population <- function( df, file,mode=c("text","genepop", "structure")[1], stratum=NULL ) {
  
  if( missing(df) )
    stop("You need to pass a data.frame to this function.")
  if( missing(file))
    stop("You need to pass a file argument to this function.")
  if( !inherits(df,"data.frame"))
    stop("This function is designed to save data frames to file")
  
  if( !is.null(stratum) & !(stratum %in% names(df) ))
    stop("You have specified a non-existant stratum to be used, how about one the data.frame actually has?")
  
  if( mode != "text" & missing(stratum))
    stop("You need to specify which stratum to use for this output file format.")
  
  if( mode == "genepop")
    file_contents <- to_genepop(df,stratum)
  else if( mode == "structure" )
    file_contents <- to_structure(df, stratum )
  else {
    file_contents <- character(nrow(df))
    for( i in 1:nrow(df))
      file_contents <- paste( as.character(df[i,]),collapse=",")
  }
  
  write(file_contents,file=file)
  invisible(0)
}