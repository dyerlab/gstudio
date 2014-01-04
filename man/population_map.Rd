\name{population_map}
\alias{population_map}
\title{Retrieve a map for a set of populations.}
\usage{
population_map(coords, map.source = "google", map.type = "terrain",
  zoom = NA)
}
\arguments{
  \item{coords}{A \code{data.frame} that has Strata,
  Latitude, and Longitude}

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

  \item{zoom}{The default zoom level when using google maps
  (default NA).  If omitted the code will attempt to define
  the map by the bounding box defined by the coords}
}
\value{
A map raster
}
\description{
This function uses the \code{ggmap} package to grab a map
of the area where the samples in the , are located.
}
\note{
This is a bit of a rough attempt and should be used only
sparingly.  I have found that the ability to get Google
Maps that cover all the sites is a bit difficult unless you
play around with the zoom option.  Google has fixed zoom
levels so you may need to try this a few times before
getting what you want.
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

