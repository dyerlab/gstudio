\name{Pe}
\alias{Pe}
\title{Polymorphic index for loci}
\usage{
Pe(x)
}
\arguments{
  \item{x}{A set of \code{locus} objects either as a vector
  or within a \code{data.frame}.}
}
\value{
The polymorphic index for the locus, sum(pi(1-pi)) as a
\code{numeric} value or as a \code{data.frame} if mutliple
loci are passed.
}
\description{
This function returns the effective number of alleles in a
sample.
}
\examples{
locus <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
Pe(locus)
locus2 <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
df <- data.frame( locus, locus2 )
Pe(df)
}

