\name{to_mv}
\alias{to_mv}
\alias{to_mv.data.frame}
\alias{to_mv.default}
\alias{to_mv.locus}
\title{Returns the passed itmes as multivariate data}
\usage{
  to_mv(x, ploidy, alleles, drop.allele = FALSE, ...)

  \method{to_mv}{default} (x, ...)

  \method{to_mv}{locus} (x, ploidy = 2, alleles = NA,
    drop.allele = FALSE, ...)

  \method{to_mv}{data.frame} (x, ploidy = 2, alleles = NA,
    drop.allele = FALSE, ...)
}
\arguments{
  \item{x}{A object to convert.}

  \item{ploidy}{The number of alleles at an individuals
  locus (default=2)}

  \item{alleles}{A set of alleles to make the matrix
  columns on.}

  \item{drop.allele}{A flag indicating an allele should be
  dropped.}

  \item{...}{Ignored}
}
\value{
  The matrix representation of \code{x}.

  Numerical value of x

  Matrix representing the locus

  A matrix
}
\description{
  This function will provide the alelles within a
  \code{locus} object for either a single locus or for a
  vector of loci
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}

  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

