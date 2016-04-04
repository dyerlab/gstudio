

#' Overload plot function
#' 
#' This function is just for quick plotting of frequencies
#' @param x A set of \code{locus} objects
#' @param mode The type of plot to create, bar or pie (bar is default).
#' @param ... Ignored
#' @method plot locus
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
plot.locus <- function( x, mode=c("bar","pie")[1], ... ) {

  f <- frequencies( x )
  Allele <- Frequency <- NULL
  if( mode=="bar")
    plot <- ggplot( f, aes(x=Allele,y=Frequency,fill=Allele)) + 
            geom_bar(stat="identity") + 
            ylim(c(0,1))
  else
    plot <- ggplot( f, aes(x=1,y=Frequency,fill=Allele)) + 
            geom_bar(stat="identity",aes(fill=Allele))  + 
            coord_polar("y") + 
            xlab("") + ylab("") + 
            theme(axis.ticks=element_blank(), axis.text.y=element_blank())
  return(plot)
}







