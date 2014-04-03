#' Plot overload
#' 
#' Plots an allele frequencies object as bar or maps.
#' @param x An object of type \code{data.frame} created from \code{frequencies}
#' @param stratum The stratum to use for calculating frequencies (default 'Population')
#' @param locus The name of the locus to use (default=NA)
#' @param longitude The name of the Longitude data column (default 'Longitude')
#' @param latitude The name of the Latitude data column (default 'Latitude')
#' @param line.color An parameter indicating the color of the border of bars and pie wedges.
#' @param label A flag indicating that the stratum names will be printed in the map plots.
#' @param palette The number of the brewer palette to use (default=8)
#' @param ... Ignored
#' @return Nothing
#' @importFrom grid grid.newpage pushViewport viewport grid.text grid.layout
#' @import ggmap
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
pies_on_map <- function( x, stratum="Population", locus=NA, longitude='Longitude', latitude='Latitude',line.color="black", label=FALSE, palette=8,... ) {
  
  if( !inherits(x,"data.frame"))
    stop("Cannot create pie charts without some data...")
  
  if( !(stratum %in% names(x)))
    stop(paste("Cannot use the stratum requested (",stratum,"), it is not in the data.frame",sep=""))
  else
    x[[stratum]] <- factor( as.character(x[[stratum]]))
  
  if( is.na(locus) | missing(locus) | !(locus %in% names(x)))
    stop(paste("Cannot use the locus requested (",locus,"), it is not in the data.frame",sep=""))
  
  freqs <- frequencies( x, loci=locus, stratum=stratum )
  coords <- strata_coordinates(x,stratum=stratum,longitude=longitude,latitude=latitude)
  ret <- Allele <- Frequency <- Stratum <- NULL  
  
  
  suppressWarnings( map <- population_map( coords, ... ) )
  bbox <- attributes(map)$bb
  
  # use subset in extent
  npop <- nrow(coords)
  coords <- coords[ (coords$Longitude >= bbox$ll.lon & coords$Longitude <= bbox$ur.lon) , ]
  coords <- coords[ (coords$Latitude >= bbox$ll.lat & coords$Latitude <= bbox$ur.lat) , ]
  if( npop > nrow(coords))
    message(paste("The google tile cut off ",(npop-nrow(coords))," locations, you must manually adjust 'zoom' parameter for this map to get all of your sites."))
  
  
  ret <- ggmap( map, extent="device" ) 
  #ret <- ret + geom_point(aes(x=Longitude,y=Latitude),data=coords) 
  #ret <- ret + geom_text(aes(x=Longitude,y=Latitude,label=Stratum),data=coords)
  
  
  coords$Latitude <- ( coords$Latitude - bbox$ll.lat ) / abs(bbox$ur.lat - bbox$ll.lat)
  coords$Longitude <- (coords$Longitude-bbox$ll.lon) / abs(bbox$ur.lon-bbox$ll.lon)
  
  vplayout <- function( x , y ) viewport(layout.pos.row=x, layout.pos.col=y)
  grid.newpage()
  pushViewport( viewport( layout=grid.layout(100,100)) )
  print(ret, vp=vplayout(1:100,1:100) )
  
  
  freqs$Allele <- factor( freqs$Allele )
  all.alleles <- levels( freqs$Allele )
  pops <- sort( unique( as.character(coords$Stratum ) ))
  for( pop in pops ){
    df <- freqs[ freqs$Stratum == pop, ] 
    lon <- coords$Longitude[ coords$Stratum==pop ]
    lat <- coords$Latitude[ coords$Stratum==pop ]
    #cat(pop,lon,lat,"\n") 
    pie <- ggplot( df, aes(y=Frequency,x="",fill=Allele)) + 
      geom_bar(width=0.75, color=line.color, stat="identity") + 
      coord_polar(theta='y') + 
      theme_nothing()
    if( length(df$Allele) < 13 )
      pie <- pie + scale_fill_brewer(type="div", palette=palette)
    print( pie, vp=viewport(x=lon,y=lat,width=0.05,height=0.05) )
             
  }
  
  if( label ) 
    grid.text( label=coords[,1], x=coords[,2]+0.011, y=coords[,3]+0.021, gp=grid::gpar(col=line.color), just="left")
  

  
  invisible(NULL)  
}
