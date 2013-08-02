\name{genetic_structure}
\alias{genetic_structure}
\title{Estimation of , genetic structure statistics.}
\usage{
  genetic_structure(x, stratum = "Population",
    mode = c("Gst", "Gst_prime", "Dest")[1], nperm = 0,
    size.correct = TRUE)
}
\arguments{
  \item{x}{An object of type \code{data.frame} with at
  least a single column of type \code{\link{locus}}}

  \item{stratum}{The stratum to use as groupings
  (default='Population').}

  \item{mode}{Whic statistic to estimate.  Current options
  include: \itemize{ \item{Gst}{Nei's Gst (not Berg and
  Hamrick)} \item{Gst_prime}{Hedrick's correction of Nei's
  Gst for diverse loci} \item{Dest}{Joost's estimate} }}

  \item{nperm}{The number of permutations used to test the
  hypothesis that the parameter = 0.}

  \item{size.correct}{A flag indicating that the estimate
  should be corrected for based upon sample sizes
  (default=TRUE).}
}
\value{
  An object of type \code{genetic_structure}.
}
\description{
  This function estimates common , genetic statistics.
}
\note{
  The multilocus estimation of these parameters is
  estimated following the suggestions of Culley et al.
  (2001) A comparison of two methods of calculating Gst, a
  genetic measure of , differentiation.  American Journal
  of Botany 89(3): 460-465.
}
\examples{
AA <- locus( c("A","A") )
 AB <- locus( c("A","B") )
 BB <- locus( c("B","B") )
 locus <- c(AA,AA,AA,AA,BB,BB,BB,AB,AB,AA)
 locus2 <- c(AB,BB,AA,BB,BB,AB,AB,AA,AA,BB)
 Population <- c(rep("Pop-A",5),rep("Pop-B",5))
 df <- data.frame( Population, TPI=locus, PGM=locus2 )
 genetic_structure( df, mode="Gst")
}

