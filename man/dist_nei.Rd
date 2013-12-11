\name{dist_nei}
\alias{dist_nei}
\title{Estimation of Bray-Curtis distance}
\usage{
dist_nei(x, stratum = "Population")
}
\arguments{
  \item{x}{A data frame with locus objects in it as well as
  a stratum object.}

  \item{stratum}{A factor indicating the stratum to use.}
}
\value{
The Nei Genetic distance
}
\description{
This function returns a measure of genetic distance based
upon the Nei's Genetic distance metric.  Assumes
drift/mutation equilibrium is the main force governing your
observed differences.
}
\note{
This is the bias corrected Nei's Standard genetic distance.
}
\examples{
AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  AD <- locus( c("A","D") )
  BC <- locus( c("B","C") )
  BD <- locus( c("B","D") )
  CC <- locus( c("C","C") )
  CD <- locus( c("C","D") )
  DD <- locus( c("D","D") )
  loc1 <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD)
  loc2 <- c(AA,AA,AC,AA,CC,CC,AC,CC,AA,AC)
  df <- data.frame( Population=c(rep("Pop-A",5),rep("Pop-B",5)), TPI=loc1, PGM=loc2 )
  dist_nei( df )
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

