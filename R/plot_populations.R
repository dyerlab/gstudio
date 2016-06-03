#' Plotting the locations of a 'population' 
#' 
#' Plots a data frame if that data frame has names indicated by the 
#'  passed arguments stratum, Longtitude, and Latitude.  Otherwise
#'  it passes the object on to plot.default()
#' @param x An object of type \code{data.frame} that has Longitude, Latitude, and Stratum variables.
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
#' @export
#' @import ggmap
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
plot_populations <- function( x, 
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
    ret <- ggmap::ggmap( map ) + ggplot2::geom_point( aes(x=Longitude, y=Latitude), data=coords, size=4, color=color ) 
    ret <- ret + ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")
    return( ret )    
  }
  
  # otherwise pass it along to the normal plot stuff
  else
    plot.default( x, ... )
}

