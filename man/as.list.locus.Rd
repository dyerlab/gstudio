\name{as.list.locus}
\alias{as.list.locus}
\title{Converts locus to a list}
\usage{
  \method{as.list}{locus} (x, ...)
}
\arguments{
  \item{x}{An object of type \code{locus}.  This can be
  either a single genotype (a rare case) or a vector of
  genotypes (preferred).}

  \item{\dots}{Additional objects that are passed to
  \code{as.data.frame.vector}.}
}
\value{
  A \code{list} object.
}
\description{
  This converts a \code{locus} object into a \code{list} so
  that you can use \code{locus} objects in a column of a
  \code{list}.
}
\examples{
loc1 <- c( locus(1:2), locus(1:2), locus( c(1,1) ) )
df <- as.list( ID=1:3, NAMED_LOCUS=loc1, loc1 )
df
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

