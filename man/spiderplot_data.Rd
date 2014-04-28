\name{spiderplot_data}
\alias{spiderplot_data}
\title{Translate paternity output into segments for geom_segments}
\usage{
spiderplot_data(pat, df, ID = "ID", OffID = "OffID",
  longitude = "Longitude", latitude = "Latitude")
}
\arguments{
  \item{pat}{The output from the \code{paterntiy()}
  function.}

  \item{df}{A \code{data.frame} with columns for the ID of
  the fathers and spatial coordinates.}

  \item{ID}{The name of the column in \code{df} that has
  the ID of the fathers (default="ID")}

  \item{OffID}{The column name in \code{df} that has the
  offspring id value}

  \item{longitude}{The label for the x-coordinate for the
  adults as it is found in the \code{df} object
  (default="Longitude")}

  \item{latitude}{The label for the y-coordinate for the
  adults as it is found in the \code{df} object
  (default="Latitude")}
}
\value{
A \code{data.frame} with the segments correctly formated
for \code{geom_segment}.
}
\description{
This is a convienence function that takes the output of a
\code{paternity()} function and the raw \code{data.frame}
with the coordinates and turns it into a \code{data.frame}
that can be used in \code{geom_segments}.
}
\author{
Rodney J. Dyer <rjdyer@vcu.edu>
}

