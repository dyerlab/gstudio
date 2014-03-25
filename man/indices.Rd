\name{indices}
\alias{indices}
\title{Returns quickly indices for a matrix of size N}
\usage{
  indices(seq, only.unique = TRUE)
}
\arguments{
  \item{seq}{The first Sequence.}

  \item{only.unique}{Makes sure to return only i,j and not
  both [i,j] and [j,i] values}
}
\value{
  A matrix of indicies
}
\description{
  This is used in conjunction with apply to get around
  doing nested loops in R.  This function is slightly
  faster than expand.grid. This function was lifted from a
  discussion on stackoverflow.
}

