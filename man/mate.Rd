\name{mate}
\alias{mate}
\title{Produces offspring}
\usage{
  mate(mom, dad, N, ID = "ID")
}
\arguments{
  \item{mom}{This is the 'maternal' individual in that all
  the metadata in the \code{data.frame} that describes this
  individual will be transfered to the offspring.}

  \item{dad}{This is the 'paternal' individual and will
  contribute only half of its genetic compliement to the
  offspring.}

  \item{N}{The number of offspring to produce.}

  \item{ID}{The column in \code{mom} that is the
  identification column as all offspring from a maternal
  individual have the same ID (see also
  \code{\link{paternity}} and \code{\link{minus_mom}}).}
}
\value{
  A \code{data.frame} of offspring.
}
\description{
  This function is mostly for simulation work and functions
  to produce a set of offpsring for the individuals passed.
}
\note{
  If only a \code{data.frame} is passed to as 'mom' and the
  rest are left out, by default the code will do a complete
  random mating (no inbreeding) process for you keeping the
  same population size as the data you passed it.  This is
  nice for iterating across generations in simulation work.
}
\examples{
f <- data.frame(Allele=LETTERS[1:2], Frequency=c(0.5,0.5))
adults <- make_population(f,N=2)
adults
mate( adults[1,], adults[2,], N=10)
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

