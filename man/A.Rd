\name{A}
\alias{A}
\title{Allelic Diversity}
\usage{
A(x, min_freq = 0)
}
\arguments{
  \item{x}{A set of \code{locus} objects}

  \item{min_freq}{The minimum frequency of allele to
  consider (default=0)}
}
\value{
The number of alleles that have \code{min_freq} frequency.
}
\description{
This function returns the number of unique, non-NA alleles,
in a sample.
}
\examples{
loci <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
A(loci)
A(loci, min_freq=0.13)
}

