\name{genetic_relatedness}
\alias{genetic_relatedness}
\title{Estimates pair-wise relatedness}
\usage{
genetic_relatedness(x, loci = NA, mode = c("Nason", "LynchRitland")[1],
  freqs = NA)
}
\arguments{
  \item{x}{A \code{data.frame} that has \code{locus}
  columns.}

  \item{loci}{The loci to use (if missing all loci are
  used).}

  \item{mode}{The kind of relatedness to be estimated.
  Currently Fij (the default) and LynchRitland are
  available.}

  \item{freqs}{An optional \code{data.frame} (as returned
  by the function \code{frequencies()} with allele
  frequencies).  If this is not provided, it will be
  estimated from all the data.  This allows you to estimate
  relatedness among subsets of individuals using more
  global measures of relatedness.}
}
\value{
A matrix of pairwise relatedness estimates.
}
\description{
This function returns single relatedness estimates as a
pairwise matrix.
}
\note{
This only works on diploid data and will return NA for any
comparison of missing genotypes.
}
\examples{
loci <- c( locus(1:2), locus(c(2,2)), locus(1:2) )
genetic_relatedness( loci )
genetic_relatedness( loci, freqs = data.frame( Allele=c("1","2"), Frequency=c(0.5,0.5)))
}

