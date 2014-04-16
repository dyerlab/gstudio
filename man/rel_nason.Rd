\name{rel_nason}
\alias{rel_nason}
\title{This estimates the Fij statistic from Nason allele-wise or for a whole locus}
\usage{
rel_nason(x, allele = NA, as.relatedness = FALSE)
}
\arguments{
  \item{x}{A vector of loci to use.}

  \item{allele}{The allele to estimate.  If this is left
  blank, a locus-wide estimator is provided.  That is the
  average of the allele-wise estimators standardized by the
  polymorphic index, \code{Pe()}.}

  \item{as.relatedness}{Return r instead of Fij
  (default=FALSE)}
}
\value{
A matrix of pairwise values for either the allele or for
the full locus.
}
\description{
This function estimates the allele-wise coancestry
statistic fij from Nason.  It can be used as a single locus
or multilocus estimator if you provide the correct
standardizations.
}
\note{
This estimator will use missing data but it treats it as if
the frequency for each allele at an individual locus are
equal to the population allele frequencies.  This may
influence your estimators.
}
\author{
Rodney J. Dyer <rjdyer@vcu.edu>
}

