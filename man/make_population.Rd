\name{make_population}
\alias{make_population}
\title{Makes a random population}
\usage{
make_population(x, N = 20)
}
\arguments{
  \item{x}{A \code{data.frame} as returned by the
  \code{frequencies()} funciton.  For a single locus, it
  has columns "Allele" and "Frequency" for multiple loci,
  it has a "Locus" column, and if it is to be subdivided
  into strata, you have a "Strata" column.}

  \item{N}{The number of individuals to create (default =
  20).  This is a 'per-stratum' estimate so if you have K
  strata the result will be K*N individuals.}
}
\value{
A \code{data.frame} with individuals, loci, and potentially
strata
}
\description{
This function takes a \code{frequencies()} object and
returns a \code{data.frame} with the appropriate stratum
and loci derived randomly.
}
\examples{
freqs <- c(0.55, 0.30, 0.15, 0.34, 0.34, 0.32)
  loci <- c(rep("TPI",3), rep("PGM",3))
  alleles <- c(LETTERS[1:3],LETTERS[8:10])
  f <- data.frame(Locus=loci, Allele=alleles, Frequency=freqs)
  make_population(f,N=20)
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

