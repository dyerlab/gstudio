\name{dist_ss}
\alias{dist_ss}
\title{Estimation of SS distance}
\usage{
dist_ss(x, stratum = "Population")
}
\arguments{
  \item{x}{A data frame with locus objects in it as well as
  a stratum object.}

  \item{stratum}{A factor indicating the stratum to use.}
}
\value{
The SS distance matrix
}
\description{
This function returns a measure of genetic distance based
upon the partitioned Sums of Squares from the AMOVA
analysis.  There are no particular assumptions to this
estimation.
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

