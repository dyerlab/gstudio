\name{migrate}
\alias{migrate}
\title{This function returns a data frame that has moved migrants}
\usage{
migrate(data, stratum = "Population", m = 0.01)
}
\arguments{
  \item{data}{A \code{data.frame} object with at least a
  stratum column.  All other columns in the data frame are
  left untouched}

  \item{stratum}{The column designating stratum.}

  \item{m}{Either a rate of migration as a numeric OR a
  migration matrix whose columns and row names are the same
  as those in the passed stratum column.}
}
\value{
A \code{data.frame} with all the data the same except for
the shuffled stratum.
}
\description{
This is a general function that moves migrants among strata
according to either a constant migration rate or a set of
migration rates defined by a migration matrix. All this
does is adjust the stratum labels of individuals, no mating
is conducted.
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

