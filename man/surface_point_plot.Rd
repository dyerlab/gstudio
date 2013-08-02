\name{surface_point_plot}
\alias{surface_point_plot}
\title{Plots a raster surface and potential coordinates using ggplot}
\usage{
  surface_point_plot(raster, coords = NA,
    is.discrete = TRUE, palette = 1, group = NA,
    x.label = "X", y.label = "Y")
}
\arguments{
  \item{raster}{An object of type \code{raster}}

  \item{coords}{A data frame of coordinates with column
  labels 'X' and 'Y'.  If this data frame has a column of
  color values labeled "Color", these will be used.
  Otherwise the points are plot using the default color
  "#E41A1C".}

  \item{is.discrete}{A logical flag to determine if the
  surface contains discrete values (default=TRUE)}

  \item{palette}{The colorbrewer palette.  If this is
  \code{is.na()} then the normal gradient colors will be
  used (from ggplot), else the colors are defined by
  \code{scale_fill_brewer()} as either 'qual' or 'seq'}

  \item{group}{An optional argument that indicates a column
  in \code{coords} that has a factor grouping for the color
  of the points to be plot (default NA).}

  \item{x.label}{The name of the label in \code{coords} to
  be used as x coordiantes (default "X")}

  \item{y.label}{The name of the label in \code{coords} to
  be used as y coordiantes (default "Y")}
}
\value{
  A ggplot object.
}
\description{
  This function plots a surface and coordinate points using
  ggplot2 and the RColorBrewer palettes.
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

