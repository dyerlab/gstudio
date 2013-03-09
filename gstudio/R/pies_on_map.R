#' Plot overload
#' 
#' Plots an allele frequencies object as bar or maps.
#' @param x An object of type \code{data.frame} containing at least one \code{locus} objects
#' @param coords A \code{data.frame} with stratum coordinates.
#' @param line.color An parameter indicating the color of the border of bars and pie wedges.
#' @param label A flag indicating that the stratum names will be printed in the map plots.
#' @param ... Ignored
#' @return Nothing
#' @export
#' @author Rodney J. Dyer <rjdyer@@vcu.edu>
pies_on_map <- function( x, coords, line.color="black", label=FALSE, ... ) {
  require( grid )
  if( !inherits(x,"data.frame"))
    stop("Cannot create pie charts without data")
  if( length(column_class(x,"locus")) < 1 )
    stop("You need to pass some loci to this function...")
  
  ret <- Allele <- Frequency <- Stratum <- NULL
  
  if( missing(coords) | is.null(coords) )
    stop("You need to provide a set of coordinates for plotting.")

  if( length(x) != 1 )
    stop( "You can only make pies_on_map for AlleleFrequency objects of a single locus.")
  
  map <- population_map( coords, ... )
  bbox <- attributes(map)$bb
  
  ret <- ggmap( map, extent="device" ) 
  coords$Longitude <- ( coords$Longitude - bbox$ll.lon ) / (bbox$ur.lon - bbox$ll.lon)
  coords$Latitude <- ( coords$Latitude - bbox$ll.lat ) / (bbox$ur.lat - bbox$ll.lat)
  
  vplayout <- function( x , y ) viewport(layout.pos.row=x, layout.pos.col=y)
  grid.newpage()
  pushViewport( viewport( layout=grid.layout(100,100) ) )
  print(ret, vp=vplayout(1:100,1:100) )
  
  
  
  
  all.alleles <- unique( unlist( alleles(x) ) )
  
  for( i in 1:length(x)){
    df <- NULL #data.frame( Allele=all.alleles, Frequency= as.numeric(get.frequencies( x[[i]], alleles=all.alleles )))
    lon <- coords[i,2]
    lat <- coords[i,3]
    pie <- ggplot( df, aes(y=Frequency,x="",fill=Allele)) + 
      geom_bar(width=1, color=line.color) + 
      coord_polar(theta='y') + 
      theme_nothing()
    print( pie, vp=viewport(x=lon,y=lat,width=0.05,height=0.05) )
  }
  
  if( label ) {
    grid.text( label=coords[,1], x=coords[,2]+0.02, y=coords[,3]+0.02, gp=gpar(col=line.color), just="left")
  }
  
  
  
  invisible(NULL)  
}
