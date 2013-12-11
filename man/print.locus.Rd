\name{print.locus}
\alias{print.locus}
\title{Overload of \code{print} function for \code{locus} objects}
\usage{
\method{print}{locus}(x, ...)
}
\arguments{
  \item{x}{The \code{locus} object}

  \item{\dots}{Other arguments passed to
  \code{\link{print.default}}.}
}
\value{
Nothing
}
\description{
Prints out the \code{locus} to stdout.
}
\examples{
loc <- locus( 1:2 )
print( loc )
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

