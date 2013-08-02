\name{allelic_diversity}
\alias{allelic_diversity}
\title{Estimates allelic diversity}
\usage{
  allelic_diversity(x, mode = c("A", "Ae", "A95")[1])
}
\arguments{
  \item{x}{A \code{locus} object or a \code{data.frame}
  with some loci in it}

  \item{mode}{The kind of diversity to estimate.  Current
  options include 'A' the total number of alleles, 'Ae' the
  effective number of alleles, and 'A95' the number of
  alleles with a frequency of at least five percent.}
}
\value{
  Numeric value for diversity
}
\description{
  This function takes a single \code{locus} object or a
  \code{data.frame} object and estimates allelic diversity
  as either the number of alleles, the effective number of
  alleles, or the number of alleles observed at a frequency
  of at least five percent.
}
\examples{
locus <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
locus2 <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
df <- data.frame( locus, locus2 )
allelic_diversity(df,mode="A")
allelic_diversity(df,mode="Ae")
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

