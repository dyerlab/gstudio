\name{dist_bray}
\alias{dist_bray}
\title{Estimation of jaccard distance}
\usage{
  dist_bray(x, stratum = "Population")
}
\arguments{
  \item{x}{Either a \code{data.frame} with both stratum and
  \code{locus} objects in them (for strata distance) OR a
  vector of \code{locus} objects and this will calculate
  distance based upon individual genetic distances.}

  \item{stratum}{The name of the stratum variable in
  \code{x}}
}
\value{
  A matrix of Jaccard distance
}
\description{
  This function returns a measure of genetic distance based
  upon the AMOVA distance metric.
}
\examples{
AA <- locus( c("A","A") )
AB <- locus( c("A","B") )
AC <- locus( c("A","C") )
BB <- locus( c("B","B") )
BC <- locus( c("B","C") )
CC <- locus( c("C","C") )
loci <- c(AA,AA,AB,AA,BB,BC,CC,BB,BB,CC)
df <- data.frame( Population=c(rep("A",5),rep("B",5) ), TPI=loci )
D <- dist_bray(df)
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

