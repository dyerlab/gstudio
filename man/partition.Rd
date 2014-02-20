\name{partition}
\alias{partition}
\title{Subdivide , into a list of substrata}
\usage{
partition(x, stratum = "Population")
}
\arguments{
  \item{x}{Any object that can be column indexed by the
  function \code{names}}

  \item{stratum}{The column name of the stratum to
  partition on (default='Population').}
}
\value{
A partitioned list of objects, indexed by name
}
\description{
This function allows you to take a single \code{Population}
object into a list of \code{Population} objects, one for
each stratum.
}
\examples{
AA <- locus( c("A","A") )
 AB <- locus( c("A","B") )
 BB <- locus( c("B","B") )
 locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
 Population <- c(rep("Pop-A",5),rep("Pop-B",5))
 locus2 <- c(AB,BB,AA,BB,BB,AB,AB,AA,AA,BB)
 df <- data.frame( Population, TPI=locus, PGM=locus2 )
 partition(df, stratum="Population")
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

