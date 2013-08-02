#' Plots a raster surface and potential coordinates using ggplot
#' 
#' This function plots a surface and coordinate points using 
#'  ggplot2 and the RColorBrewer palettes.
#' @param raster An object of type \code{raster}
#' @param coords A data frame of coordinates with column labels
#'  'X' and 'Y'.  If this data frame has a column of color values 
#'  labeled "Color", these will be used.  Otherwise the points are
#'  plot using the default color "#E41A1C".
#' @param is.discrete A logical flag to determine if the surface
#'  contains discrete values (default=TRUE)
#' @param palette The colorbrewer palette.  If this is \code{is.na()}
#'  then the normal gradient colors will be used (from ggplot), else
#'  the colors are defined by \code{scale_fill_brewer()} as either 
#'  'qual' or 'seq'
#' @param group An optional argument that indicates a column in \code{coords}
#'  that has a factor grouping for the color of the points to be plot 
#'  (default NA).
#' @param x.label The name of the label in \code{coords} to be used as x 
#'  coordiantes (default "X")
#' @param y.label The name of the label in \code{coords} to be used as y 
#'  coordiantes (default "Y")
#' @return A ggplot object.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @export
#' 
surface_point_plot <- function( raster, coords = NA, is.discrete=TRUE, palette=1, group=NA, x.label="X", y.label="Y" ) {
  require(ggplot2)
  
  X <- Y <- Group <- Value <- NULL
  
  if( !inherits( raster, "RasterLayer") )
    stop("This function requires a raster object.")

  if( !inherits( coords, "data.frame") )
    stop("The parameter 'coords' must be of type 'data.frame'.")
  
  points <- data.frame( X=coords[[x.label]], Y=coords[[y.label]] )
  if( !is.na(group) )
    points$Group <- coords[[group]]
  
  df <- data.frame(raster:::rasterToPoints( raster ))
  names( df ) <- c("X","Y","Value")
  
  if(is.discrete)
    df$Value <- as.factor( df$Value )

  p <- ggplot() + geom_tile( aes(x=X,y=Y,fill=Value ), data=df) + theme_bw() + coord_equal()

  if( !is.na( palette ) ) {
    
    if( is.discrete )
      p <- p + scale_fill_brewer(type="qual", palette=palette)
    
    else
      p <- p + scale_fill_brewer("seq", palette=palette)    
  
  }
  else
    p <- p + scale_fill_gradient()
  
  if(!all(is.na(coords)) ) {
    
    if( !is.na(group) && group %in% names(coords) )
      p <- p + geom_point(aes(x=X,y=Y, color=Group), data=points, size=3 )
    
    else
      p <- p + geom_point(aes(x=X,y=Y), data=points, size=3, color="#e41a1c" )
  }
  
  return( p )
} 
