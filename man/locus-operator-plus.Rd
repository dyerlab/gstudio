\name{+.locus}
\alias{+.locus}
\title{Overload '+' operator for pairs of \code{locus} objects}
\usage{
\method{+}{locus}(e1, e2)
}
\arguments{
  \item{e1}{A \code{locus} object.}

  \item{e2}{A \code{locus} object.}
}
\value{
A new \code{locus} object that represents an offspring
genotype.
}
\description{
An overload of the \code{+} operator for \code{locus}
objects that results in the creation of an offspring
\code{locus}.
}
\examples{
dad <- locus( c("A", "A") )
mom <- locus( c("B", "B") )
off <- mom + dad
off
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

