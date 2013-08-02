\name{strata_coordinates}
\alias{strata_coordinates}
\title{Grab coordinates for strata}
\usage{
  strata_coordinates(x, stratum = "Population",
    longitude = "Longitude", latitude = "Latitude")
}
\arguments{
  \item{x}{A \code{data.frame} object.}

  \item{stratum}{The name of the stratum to partition on
  (default="Population").}

  \item{longitude}{The column name of the longitude}

  \item{latitude}{The column name of the latitude}
}
\value{
  A data frame, with Stratum Latitude and Longitude,
  summarized by center of each stratum.
}
\description{
  This function takes a , and a stratum and makes a data
  frame consisting of Stratum, Latitide, and Longitude for
  each stratum
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

