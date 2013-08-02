#' Retrieve a map for a set of populations.
#' 
#' This function uses the \code{ggmap} package to grab a map of the area where the
#'  samples in the , are located.
#' @note This is a bit of a rough attempt and should be used only sparingly.  I have found 
#'  that the ability to get Google Maps that cover all the sites is a bit difficult 
#'  unless you play around with the zoom option.  Google has fixed zoom levels so you may
#'  need to try this a few times before getting what you want.
#' @param coords A \code{data.frame} that has Strata, Latitude, and Longitude
#' @param map.source Where the map should be retrieved from. Common options include
#'  \itemize{
#'    \item{google}{Get the map from Google Maps (this is the default)}
#'    \item{osm}{Open Streat Map derived}
#' }
#' @param map.type What kind of map to use.  Current types include:
#' \itemize{
#'  \item{sattelite}{A satellite image of the area.}
#'  \item{terrain}{A stylized topological map (this is the default).}
#'  \item{road}{The default google road map.}
#'  \item{hybrid}{Mix of road and terrain maps.}
#' }
#' @param zoom The default zoom level when using google maps (default NA).  If omitted
#'  the code will attempt to define the map by the bounding box defined by the coords
#' @return A map raster
#' @export
#' @import ggmap
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' 
population_map <- function( coords, map.source="google", map.type="terrain", zoom=NA ){
  Longitude <- Latitude <- NULL
  
  if( is.na(zoom) ) {
    location <- make_bbox( Longitude, Latitude, data=coords, f=0 )
    lt.rng <- abs( location[1]-location[3] ) * 0.2
    ln.rng <- abs( location[2]-location[4] ) * 0.2
    location <- location + c(-lt.rng,-ln.rng,lt.rng,ln.rng)
    map <- get_map( location=location, maptype=map.type, source=map.source, filename=tempfile() )    
  }
  else {
    location <- c( lon=mean(coords$Longitude),lat=mean(coords$Latitude ) )
    if( !is.numeric( zoom ) )
      zoom <- as.numeric( zoom )
    map <- get_map( location=location, maptype=map.type, source=map.source, zoom=zoom, filename=tempfile() )
  }
  
  return( map )
}

