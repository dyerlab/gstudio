\name{genetic_diversity}
\alias{genetic_diversity}
\title{Estimate genetic diversity among strata in a Population,}
\usage{
genetic_diversity(x, stratum = NULL, mode = c("A", "Ae", "A95", "He", "Ho",
  "Fis")[2])
}
\arguments{
  \item{x}{A \code{data.frame} object with
  \code{\link{locus}} columns.}

  \item{stratum}{The strata by which the genetic distances
  are estimated.  This can be an optional parameter when
  estimating distance measures calculated among indiviudals
  (default='Population').}

  \item{mode}{The particular genetic diversity metric that
  you are going to use.  The \code{gstudio} package
  currently includes the following individual distance
  measures: \describe{ \item{A}{Number of alleles}
  \item{Ae}{Effective number of alleles (default)}
  \item{A95}{Number of alleles with frequency at least five
  percent} \item{He}{Expected heterozygosity}
  \item{Ho}{Observed heterozygosity} \item{Fis}{Wright's
  Inbreeding coefficient (size corrected).} }}
}
\value{
A \code{data.frame} with columns for strata, diversity
(mode), and potentially P(mode=0).
}
\description{
This function is the main one used for estimating genetic
diversity among stratua.  Given the large number of genetic
diversity metrics, not all potential types are included.
}
\examples{
AA <- locus( c("A","A") )
 AB <- locus( c("A","B") )
 BB <- locus( c("B","B") )
 locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
 locus2 <- c(AB,BB,AA,BB,BB,AB,AB,AA,AA,BB)
 Population <- c(rep("Pop-A",5),rep("Pop-B",5))
 df <- data.frame( Population, TPI=locus, PGM=locus2 )
 genetic_diversity( df, mode="Ae")
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

