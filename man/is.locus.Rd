\name{is.locus}
\alias{is.locus}
\title{An 'is-a' operator for \code{locus}}
\usage{
is.locus(x)
}
\arguments{
  \item{x}{An object to query}
}
\value{
A logical flag indicating if \code{x} is a type of
\code{locus}
}
\description{
A quick convienence function to determine if an object is
inherited from the \code{locus} object.
}
\examples{
loc1 <- locus( c("A","A") )
is.locus( loc1 )
is.locus( FALSE )
is.locus( 23 )
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

