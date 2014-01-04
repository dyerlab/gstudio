\name{strata_distance}
\alias{strata_distance}
\title{Return distances between strats}
\usage{
strata_distance(x, mode = c("Euclidean", "Circle")[2])
}
\arguments{
  \item{x}{A \code{data.frame} object from
  \code{strata_coordinates}.}

  \item{mode}{The type of distance to calculate.  Currently
  there are two available types of physical distance,
  Euclidean (straight-line) and Great Circle (from the
  curvature of the earth)}
}
\value{
A data frame, with Stratum Latitude and Longitude,
summarized by center of each stratum.
}
\description{
This function takes a \code{data.frame} from the
\code{strata_coordinates} function and returns the between
strata distance matrix.
}
\note{
This uses the mean radius of the earth to be 6371km as the
latest estimate by USGS.
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

