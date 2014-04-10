\name{Fij}
\alias{Fij}
\title{This estimates Fij allele-wise}
\usage{
Fij(loci, allele = NA)
}
\arguments{
  \item{loci}{A vector of loci to use.}

  \item{allele}{The allele to estimate.  If this is left
  blank, a locus-wide estimator is provided.  That is the
  average of the allele-wise estimators standardized by the
  polymorphic index, \code{Pe()}.}
}
\value{
A matrix of pairwise values for either the allele or
}
\description{
This function estimates the allele-wise coancestry
statistic fij from Nason.  It can be used as a single locus
or multilocus estimator if you provide the correct
standardizations.
}

