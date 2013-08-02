\name{transition_probability}
\alias{transition_probability}
\title{Returns transition probability for offspring given one or more parents.}
\usage{
  transition_probability(off, mom, dad)
}
\arguments{
  \item{off}{A locus from the offspring}

  \item{mom}{A locus from one parent}

  \item{dad}{A locus from another parent}
}
\value{
  The numeric probability of the offspring given the
  parents
}
\description{
  This is a quick function used in paternity testing that
  provides the probability of transition for a locus of any
  ploidy level.
}
\examples{
off <- locus( 1:2 )
mom <- locus( c(1,1) )
dad1 <- locus( c(2,2) )
dad2 <- locus( c(1,2) )
dad3 <- locus( c(1,1) )
transition_probability( off, mom, dad1 )
transition_probability( off, mom, dad2 )
transition_probability( off, mom, dad3 )
}
\author{
  Rodney J. Dyer <rjdyer@vcu.edu>
}

