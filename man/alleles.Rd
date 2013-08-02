\name{alleles}
\alias{alleles}
\alias{alleles.default}
\alias{alleles.locus}
\title{Returns the alleles in a \code{locus} object}
\usage{
  alleles(x, all = TRUE)

  \method{alleles}{default} (x, all = TRUE)

  \method{alleles}{locus} (x, all = TRUE)
}
\arguments{
  \item{x}{A \code{locus} object (single or vector)}

  \item{all}{A flag indicating that you want all alleles
  returned (default=TRUE) A \code{FALSE} flag here
  indicates that you only want the unique alleles returned
  (e.g., the ones present).}
}
\value{
  A matrix of alleles.  If \code{x} is a vector then the
  result will be represented by rows.

  Names from passed vector

  Either a \code{matrix} or \code{vector} of alleles
  depending upon what was passed to the function.
}
\description{
  This function will provide the alelles within a
  \code{locus} object for either a single locus or for a
  vector of loci
}
\examples{
locus <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
alleles(locus)
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

