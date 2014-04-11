\name{rep.locus}
\alias{rep.locus}
\title{Replicate a locus}
\usage{
\method{rep}{locus}(x, times, ...)
}
\arguments{
  \item{x}{An object of type character}

  \item{times}{The number of times to replicate this}

  \item{...}{Ignored}
}
\value{
A vector of \code{locus} objects
}
\description{
A quick replacement for \code{rep} so it does not replicate
a \code{locus} object as a character so we can use
functions like \code{outer} efficiently.
}
\examples{
loc1 <- locus( c("A","B"))
rep(loc1, times=4)
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

