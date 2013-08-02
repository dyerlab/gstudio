\name{as.locus}
\alias{as.locus}
\title{As operator for locus}
\usage{
  as.locus(x)
}
\arguments{
  \item{x}{An object that is to be truned into a
  \code{locus}.}
}
\value{
  An object of type \code{locus}
}
\description{
  This takes several things and shoves it into the
  constructor
}
\examples{
lst <- list( "A", "B" )
as.locus( lst )
vec <- 1:2
as.locus( lst )
chr <- "A"
as.locus( chr )
chr.sep <- "A:A"
as.locus( chr )
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}
\seealso{
  \code{\link{locus}}
}

