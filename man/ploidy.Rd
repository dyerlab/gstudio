\name{ploidy}
\alias{ploidy}
\title{Returns the number of alleles in a \code{locus} object}
\usage{
ploidy(x)
}
\arguments{
  \item{x}{A \code{locus} object (single or vector)}
}
\value{
A count of the number of alleles in the \code{locus}
}
\description{
This function will a count of the number of alelles within
a \code{locus} object for either a single locus or for a
vector of loci
}
\examples{
loc <- locus( 1:2 )
ploidy( loc )
loci <- c( locus(1:2), locus(c(1,1) ) )
ploidy( loci )
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

