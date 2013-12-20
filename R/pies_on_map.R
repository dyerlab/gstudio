#' Plot overload
#' 
#' Plots an allele frequencies object as bar or maps.
#' @param x An object of type \code{data.frame} created from \code{frequencies}
#' @param stratum The stratum to use for calculating frequencies (default 'Population')
#' @param locus The name of the locus to use (default=NA)
#' @param Longitude The name of the Longitude data column (default 'Longitude')
#' @param Latitude The name of the Latitude data column (default 'Latitude')
#' @param line.color An parameter indicating the color of the border of bars and pie wedges.
#' @param label A flag indicating that the stratum names will be printed in the map plots.
#' @param palette The number of the brewer palette to use (default=8)
#' @param ... Ignored
#' @return Nothing
#' @importFrom grid grid.newpage pushViewport viewport grid.text grid.layout
#' @import ggmap
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
pies_on_map <- function( x, stratum="Population", locus=NA, Longitude='Longitude', Latitude='Latitude',line.color="black", label=FALSE, palette=8,... ) {
  
  if( !inherits(x,"data.frame"))
    stop("Cannot create pie charts without some data...")
  
  if( missing(stratum) | !(stratum %in% names(x)))
    stop(paste("Cannot use the stratum requested (",stratum,"), it is not in the data.frame",sep=""))

  if( is.na(locus) | missing(locus) | !(locus %in% names(x)))
    stop(paste("Cannot use the locus requested (",locus,"), it is not in the data.frame",sep=""))
  
  freqs <- frequencies( x, stratum=stratum, loci=locus )
  coords <- strata_coordinates(x,stratum=stratum,longitude=Longitude,latitude=Latitude)
  ret <- Allele <- Frequency <- Stratum <- NULL  
  
  suppressWarnings( map <- population_map( coords, ... ) )
  bbox <- attributes(map)$bb
  
  ret <- ggmap( map, extent="device" ) 
  coords$Longitude <- ( coords$Longitude - bbox$ll.lon ) / (bbox$ur.lon - bbox$ll.lon)
  coords$Latitude <- ( coords$Latitude - bbox$ll.lat ) / (bbox$ur.lat - bbox$ll.lat)
  
  vplayout <- function( x , y ) viewport(layout.pos.row=x, layout.pos.col=y)
  grid.newpage()
  pushViewport( viewport( layout=grid.layout(100,100) ) )
  print(ret, vp=vplayout(1:100,1:100) )
  
  freqs$Allele <- factor( freqs$Allele )
  all.alleles <- levels( freqs$Allele )
  pops <- sort( unique( freqs$Stratum ) )
  for( pop in pops ){
    df <- freqs[ freqs$Stratum == pop, ] 
    lon <- coords$Longitude[ coords$Stratum==pop]
    lat <- coords$Latitude[ coords$Stratum==pop]
    pie <- ggplot( df, aes(y=Frequency,x="",fill=Allele)) + 
      geom_bar(width=0.75, color=line.color, stat="identity") + 
      coord_polar(theta='y') + 
      theme_nothing()
    if( length(df$Allele) < 13 )
      pie <- pie +  scale_fill_brewer(type="div", palette=palette)
    print( pie, vp=viewport(x=lon,y=lat,width=0.05,height=0.05) )
  }
  
  if( label ) 
    grid.text( label=coords[,1], x=coords[,2]+0.02, y=coords[,3]+0.02, gp=grid::gpar(col=line.color), just="left")
  
  
  
  
  invisible(NULL)  
}
