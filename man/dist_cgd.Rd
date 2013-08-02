\name{dist_cgd}
\alias{dist_cgd}
\title{Estimation of conditional genetic distance}
\usage{
  dist_cgd(x, stratum = "Population")
}
\arguments{
  \item{stratum}{The groups among which you are going to
  estimate genetic distances.}

  \item{x}{The genetic data, either as a single locus or
  multilocus (\code{data.frame}) object.}
}
\value{
  A matrix of conditional genetic distance estimates.
}
\description{
  This function returns a measure of conditional genetic
  distance based upon the Population Graphs approach from
  Dyer & Nason (2004) and Dyer et al. (2010).
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

