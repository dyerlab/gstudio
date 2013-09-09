\name{geom_strata}
\alias{geom_strata}
\title{Returns ggplot layer for population data}
\usage{
  geom_strata(mapping = NULL, data = NULL, ...)
}
\arguments{
  \item{mapping}{The aesthetic mapping, this MUST have
  values for x, y, and stratum}

  \item{data}{The \code{data.frame} from which the
  coordinates and other materials are to be pulled.}

  \item{...}{Other parameters submitted to
  \code{geom_point()}.}
}
\description{
  This function takes a \code{data.frame} of population
  data and returns a \code{geom_point()} layer for
  plotting.  You can also indicate shape and color
  attributes in the mapping (through \code{aes}) that will
  be carried through.
}
\examples{
require(ggplot2)
data(arapat)
ggplot() + geom_strata( aes(x=Longitude,y=Latitude,stratum=Population), data=arapat) + coord_equal()
s <- geom_strata( aes(x=Longitude,y=Latitude,stratum=Population, color=Cluster, shape=Species), data=arapat)
ggplot() + s + coord_equal()
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

