#' Changes a frequency data.frame into a table format
#' 
#' This is a simple function that takes a frequency table and turns it into a
#'  tabular format with rows for each stratum and 
#'  
#'  


frequency_table <- function( x, stratum="Population", loci=NULL) {
  locus_names <- column_class(x,"locus")
  if( length( locus_names) < 1 ) 
    stop("You need to have some locus objects in your data frame.")
  if( length( locus_names ) == 1 )
    loci <- locus_names
  
  if( is.null(loci) || !(loci %in% names(x))  )
    stop("You must specify which locus to use and it has to be in your data.frame ...")


  freqs <- frequencies(x,stratum=stratum,loci=loci)
  alleles <- unique(freqs$Allele)
  populations <- 
}