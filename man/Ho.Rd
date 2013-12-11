\name{Ho}
\alias{Ho}
\title{Estimate observed heterozygosity}
\usage{
Ho(x)
}
\arguments{
  \item{x}{An object of type \code{locus}}
}
\value{
The expected heterozygosity
}
\description{
Returns the general observed heterozygosity parameter from
the frequencies
}
\examples{
loci <- c( locus( c("A","A") ), locus( c("A","A") ), locus( c("A","B") ) )
Ho( loci )
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

