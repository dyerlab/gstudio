\name{optimal_sampling}
\alias{optimal_sampling}
\title{Estimate optimal sampling allocation}
\usage{
optimal_sampling(N, phi)
}
\arguments{
  \item{N}{The total sample size (number of strata * number
  of individuals pre stratum)}

  \item{phi}{An estimate of the level of differentiation.}
}
\value{
A ggplot object with a plot of the variance in phi due to
different sample sizes with a region of +/- 10% of the
provided phi value
}
\description{
This function returns a graphical representation of optimal
sampling allocations given an estimate of the total number
of samples you are looking at genotyping and the amount of
expected differentiation among strata.
}
\examples{
N <- 400
Phi <- 0.25
optimal_sampling( N, Phi )
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

