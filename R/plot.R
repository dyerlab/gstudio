#' Plotting the locations of a 'population' 
#' 
#' Plots a data frame if that data frame has names indicated by the 
#'  passed arguments stratum, Longtitude, and Latitude.  Otherwise
#'  it passes the object on to plot.default()
#' @param x An object of type \code{data.frame}
#' @param stratum The stratum to plot (default='Population').
#' @param longitude Name of the column to be used as a decimal longitude
#' @param latitude Name of the column to be used as decimal latitude
#' @param map.source Where the map should be retrieved from. Common options include
#'  \describe{
#'    \item{google}{Get the map from Google Maps (this is the default)}
#'    \item{osm}{Open Streat Map derived}
#' }
#' @param map.type What kind of map to use.  Current types include:
#' \describe{
#'  \item{sattelite}{A satellite image of the area.}
#'  \item{terrain}{A stylized topological map (this is the default).}
#'  \item{road}{The default google road map.}
#'  \item{hybrid}{Mix of road and terrain maps.}
#' }
#' @param color The color of the markers to be plot.
#' @param zoom The default zoom level when using google maps (default NA).  If omitted
#'  the code will attempt to define the map by the bounding box defined by the coords
#' @param ... Ignored
#' @return A ggplot object that will be plotted by default.
#' @method plot data.frame
#' @export
#' @import ggmap
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
plot.data.frame <- function( x, 
                             stratum="Population", 
                             longitude="Longitude", latitude="Latitude",  
                             map.source="google", map.type="terrain", 
                             color="black", zoom=NA,... ) {
  
  # if it has stratum and lat/lon, plot it in ggmap
  if( (stratum %in% names(x)) && 
        (longitude %in% names(x)) && 
        (latitude %in% names(x) ) ) {
    args <- list(...)  
    Longitude <- Latitude <- NULL
    coords <- strata_coordinates( x , stratum, longitude, latitude )
    map <- population_map( coords, map.source, map.type, zoom )
    ret <- ggmap( map ) + geom_point( aes(x=Longitude, y=Latitude), data=coords, size=4, color=color ) 
    ret <- ret + xlab("Longitude") + ylab("Latitude")
    return( ret )    
  }
  
  # otherwise pass it along to the normal plot stuff
  else
    plot.default( x, ... )
}



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
  #require(ggplot2)
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







