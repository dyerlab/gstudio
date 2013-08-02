\name{pies_on_map}
\alias{pies_on_map}
\title{Plot overload}
\usage{
  pies_on_map(x, stratum = "Population", locus = NA,
    Longitude = "Longitude", Latitude = "Latitude",
    line.color = "black", label = FALSE, ...)
}
\arguments{
  \item{x}{An object of type \code{data.frame} created from
  \code{frequencies}}

  \item{stratum}{The stratum to use for calculating
  frequencies (default 'Population')}

  \item{locus}{The name of the locus to use (default=NA)}

  \item{Longitude}{The name of the Longitude data column
  (default 'Longitude')}

  \item{Latitude}{The name of the Latitude data column
  (default 'Latitude')}

  \item{line.color}{An parameter indicating the color of
  the border of bars and pie wedges.}

  \item{label}{A flag indicating that the stratum names
  will be printed in the map plots.}

  \item{...}{Ignored}
}
\value{
  Nothing
}
\description{
  Plots an allele frequencies object as bar or maps.
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

