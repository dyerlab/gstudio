\name{plot.data.frame}
\alias{plot.data.frame}
\title{Plotting the locations of a 'population'}
\usage{
\method{plot}{data.frame}(x, stratum = "Population",
  longitude = "Longitude", latitude = "Latitude", map.source = "google",
  map.type = "terrain", color = "black", zoom = NA, ...)
}
\arguments{
  \item{x}{An object of type \code{data.frame}}

  \item{stratum}{The stratum to plot
  (default='Population').}

  \item{longitude}{Name of the column to be used as a
  decimal longitude}

  \item{latitude}{Name of the column to be used as decimal
  latitude}

  \item{map.source}{Where the map should be retrieved from.
  Common options include \describe{ \item{google}{Get the
  map from Google Maps (this is the default)}
  \item{osm}{Open Streat Map derived} }}

  \item{map.type}{What kind of map to use.  Current types
  include: \describe{ \item{sattelite}{A satellite image of
  the area.} \item{terrain}{A stylized topological map
  (this is the default).} \item{road}{The default google
  road map.} \item{hybrid}{Mix of road and terrain maps.}
  }}

  \item{color}{The color of the markers to be plot.}

  \item{zoom}{The default zoom level when using google maps
  (default NA).  If omitted the code will attempt to define
  the map by the bounding box defined by the coords}

  \item{...}{Ignored}
}
\value{
A ggplot object that will be plotted by default.
}
\description{
Plots a data frame if that data frame has names indicated
by the passed arguments stratum, Longtitude, and Latitude.
Otherwise it passes the object on to plot.default()
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

