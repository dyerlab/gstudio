\name{rel_lynch}
\alias{rel_lynch}
\title{This estimates the Lynch & Ritland (1999)}
\usage{
rel_lynch(x)
}
\arguments{
  \item{x}{A \code{data.frame} or \code{vector} that has
  \code{locus} objects in it.  If you pass it a
  \code{data.frame} it will return the multilocus
  relatedness.}
}
\value{
A matrix of pairwise values
}
\description{
This function estimates the allele-wise relatedness
statistic from Lynch & Ritland (1999). It is very sensitive
to the presence of rare alleles.
}
\details{
This is an symetric estimator using the formula:

\deqn{\hat{r}_{XY} = \frac{r_{XY} + r_{YX}}{2}}

where

\deqn{r_{XY} = \frac{1}{\sum_{i=1}^L w_i} \sum_{i=1}^L
\frac{p_a(\delta_{bc}+\delta_{bd}) + p_b(\delta_{ac} +
\delta_{ad}) - 4p_a p_b}{2p_a p_b} }

and

\deqn{w_i = \frac{((1+\delta_{ab})(p_a+p_b) - 4*p_a
p_b)}{2p_a p_b}}

For missing genotypes, relatedness is not estimated for any
pair-wise comparison.  This does not constitute an error,
specifically, but you should be careful of noting which
comparisons are made from no or at least fewer than the
full set of loci.
}
\note{
For missing data and for data comparing heterozygotes at a
2-allele locus this estimator is undefined.
}
\author{
Rodney J. dyer <rjdyer@vcu.edu>
}

