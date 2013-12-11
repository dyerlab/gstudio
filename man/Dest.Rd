\name{Dest}
\alias{Dest}
\title{Estimation Joost's Dest parameter}
\usage{
Dest(x, stratum = "Population", nperm = 0, size.correct = FALSE)
}
\arguments{
  \item{x}{A vector of \code{\link{locus}} objects or a
  \code{data.frame} with \code{locus} objects.}

  \item{stratum}{Either a vector of strata variables if
  \code{x} is a \code{locus} vector or the name of the
  column representing strata in \code{x} if it is a
  \code{data.frame}.}

  \item{nperm}{The number of permutations to run for
  significance of the estimator.}

  \item{size.correct}{A flag indicating that the estimate
  should be corrected for based upon sample sizes
  (default=TRUE).}
}
\value{
A \code{data.frame} with Dest, Hs, Ht, and P (if asked
for).  When multiple loci are provided, the results also
provide a multilocus estimate using the harmonic mean.
}
\description{
This function estimates the parameter (and potentially the
confidence surrounding its value) for Joost's Dest.
}
\examples{
a1 <- sample( LETTERS[1:5], size=20, replace=TRUE)
 a2 <- sample( LETTERS[4:8], size=20, replace=TRUE)
 raw_alleles <- matrix( c(a1,a2), ncol=2, byrow=TRUE )
 locus <- locus( raw_alleles, type="column")
 Population <- c(rep("Pop-A",10),rep("Pop-B",10))
 Dest( locus, Population )
 a1 <- sample( LETTERS[1:5], size=20, replace=TRUE)
 a2 <- sample( LETTERS[4:8], size=20, replace=TRUE)
 raw_alleles <- matrix( c(a1,a2), ncol=2, byrow=TRUE )
 locus2 <- locus( raw_alleles, type="column")
 df <- data.frame( Population, TPI=locus, PGM=locus2 )
 Dest( df, nperm=99)
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

