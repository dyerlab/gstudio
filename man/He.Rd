\name{He}
\alias{He}
\title{Estimate expected heterozygosity}
\usage{
  He(x, small.sample.correction = FALSE)
}
\arguments{
  \item{x}{Either a \code{data.frame} object with
  \code{locus} objects or a vector or \code{locus}
  objects.}

  \item{small.sample.correction}{Apply the 2N/(2N-1)
  correction to the data for small sample sizes.}
}
\value{
  The expected heterozygosity as a numeric or a
  \code{data.frame} if several loci are passed.
}
\description{
  Returns the general expected heterozygosity parameter
  from the frequencies
}
\examples{
loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
He( loci )
He( loci, small.sample.correction=TRUE )
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

