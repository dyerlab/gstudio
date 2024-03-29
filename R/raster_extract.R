#' Extracts values from a raster at Points
#' 
#' This is s quick convenience function that takes a `data.frame` and
#'   a raster and returns the extracted values as a single column.
#' @param x A data frame object.
#' @param raster A raster from which you can extract the values
#' @param Latitude The name of the column holding the latitude values (default = "Latitude")
#' @param Longitude The name of the column holding the longitude (default = "Longitude")
#' @param CRS The coordinate reference system (default = 4326)
#' @importFrom methods as
#' @export 
raster_extract <- function( x, raster, Latitude = "Latitude", Longitude = "Longitude", CRS = 4326 ) {
  if( !( Latitude %in% names(x) ) || !( Longitude %in% names(x)) )  {
    stop("The columns you need for Lat/Lon are not in the data.frame you are passing...")
  }
  df <- sf::st_as_sf( x, coords=c(Longitude,Latitude), crs=CRS ) 
  
  if( sf::st_crs(raster) != sf::st_crs(df) ){
    stop("You must have the data and the raster in the same projection... duh")
  }
  vals <- raster::extract( raster, as(df,"Spatial"))
  return( vals )
}
