\name{rel_queller}
\alias{rel_queller}
\title{This estimates the Queller & Goodnight (1989)}
\usage{
rel_queller(x)
}
\arguments{
  \item{x}{A \code{data.frame} or \code{vector} that has
  \code{locus} objects in it.  If you pass it a
  \code{data.frame} it will return the multilocus
  relatedness.}
}
\value{
A matrix of pairwise relatedness values.
}
\description{
This function estimates the allele-wise relatedness
statistic from Queller & Goodnight (1989).
}
\details{
This is an assymetric estimator using the formula:

\deqn{ r_{XY} = \frac{\sum_{i=1}^L(\delta_{ac} +
\delta_{ad} + \delta_{bd} + \delta_{bd} -p_a - p_b - p_c -
p_d) }{\sum_{i=1}^L (2 + \delta_{ab} + \delta_{cd} - p_a -
p_b - p_c - p_d)} }
}
\note{
For missing data and for data comparing heterozygotes at a
2-allele locus this estimator is undefined.  Moreover, I do
add zero to both the numerator
}
\author{
Rodney J. dyer <rjdyer@vcu.edu>
}

