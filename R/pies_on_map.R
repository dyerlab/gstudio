#' Create a pie chart map of allele frequencies on google maps.
#' 
#' This function takes a data.frame and makes a set of pie charts for allele frequencies 
#' @param x An object of type \code{data.frame} containing \code{locus} objects.
#' @param stratum The stratum to use for calculating frequencies (default 'Population')
#' @param locus The name of the locus to use (required)
#' @param longitude The name of the Longitude data column (default 'Longitude')
#' @param latitude The name of the Latitude data column (default 'Latitude')
#' @param max.rad The maximum radius size for the pie charts (in real space) as meters.  By 
#'  default, it takes the maximum distance between stratum divided by 50.  They do not change
#'  by zooming in and out, so you can 'tune' it in here.
#' @param ... Additional arguments to plotGoogleMaps function.
#' @return Nothing
#' @importFrom plotGoogleMaps pieSP plotGoogleMaps
#' @importFrom ggmap has_google_key google_key
#' @importFrom sp SpatialPointsDataFrame proj4string<- CRS
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples 
#' \dontrun{
#' data(arapat)
#' pies_on_map(arapat,locus="LTRS")
#' }
pies_on_map <- function( x, stratum="Population", locus=NULL, longitude='Longitude',latitude='Latitude', max.rad=NULL, ...) {
  if( !is(x,"data.frame"))
    stop("Please pass a data frame to the pies_on_map() function.")
  if( is.null(locus) || !(locus %in% names(x)))
    stop("You must specify a locus to use in the analysis")
  if( !(longitude %in% names(x)) || !(latitude %in% names(x)))
    stop("You must specify the columns representing latitude and longitude in the data.frame")
  if( !(stratum %in% names(x))) 
    stop("You must specify a population column in the data.frame to use for coordinates")
  
  
  if( !has_google_key() ) {
    stop("To use the Google Javascript API, you must have an API key.  See ?ggmap::register_google for more information.")
  }
  
  pts <- strata_coordinates(x,stratum=stratum,longitude=longitude,latitude=latitude,as.SpatialPoints = TRUE)
  freqs <- frequency_matrix(x,stratum=stratum,loci=locus)
  col_names <- names(freqs)[2:ncol(freqs)]
  
  data <- SpatialPointsDataFrame( pts, freqs)
  proj4string(data) <- CRS('+init=epsg:4326')
  
  
  if( is.null( max.rad) ) {
    d <- strata_distance(strata_coordinates(x,stratum=stratum,longitude=longitude,latitude=latitude))
    max.rad <- max(d)/50*1000
  }
  
  pies <- pieSP(data,zcol=col_names,max.radius=max.rad)
  pies$allele <- rep(col_names,nrow(freqs))
  
  
  api <- paste( "https://maps.googleapis.com/maps/api/js?key=",
                google_key(),
                "&callback=initMap", sep="" )
  
  m <- plotGoogleMaps(pies,zcol='allele', api=api, ...)
  invisible(NULL)
  
}
