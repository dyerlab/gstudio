\name{frequencies}
\alias{frequencies}
\alias{frequencies.data.frame}
\alias{frequencies.default}
\alias{frequencies.locus}
\title{Get allele frequencies}
\usage{
frequencies(x, loci, stratum, ...)

\method{frequencies}{default}(x, ...)

\method{frequencies}{locus}(x, loci, ...)

\method{frequencies}{data.frame}(x, loci, stratum, ...)
}
\arguments{
  \item{x}{Either a vector of types \code{locus} or a
  \code{data.frame} containing \code{locus} objects.}

  \item{loci}{The loci to get frquencies from (default is
  all).}

  \item{stratum}{Partition of the data based upon this
  stratum (default is no partitions).}

  \item{...}{Ignored}
}
\value{
A data frame with Frequencies, Alleles, Loci, and perhaps
Stratum columns (Allele and Frquencies are at a minimium).

A data frame with Frequencies, Alleles, Loci, and perhaps
Stratum columns (Allele and Frquencies are at a minimium).

A data frame with Frequencies, Alleles, Loci, and perhaps
Stratum columns (Allele and Frquencies are at a minimium).

A data frame with Frequencies, Alleles, Loci, and perhaps
Stratum columns (Allele and Frquencies are at a minimium).
}
\description{
This function converts loci into data frames consisting of
alleles, stratum, frequencies, etc.
}
\examples{
AA <- locus( c("A","A") )
AB <- locus( c("A","B") )
BB <- locus( c("B","B") )
loc1 <- c(AA,AB,AB,AA,BB)
frequencies(loc1)
loc2 <- c(BB,BB,AB,AA,BB)
df <- data.frame(Population=c(rep("A",3), rep("B",2)), TPI=loc1, PGM=loc2)
frequencies(df)
frequencies(df,stratum="Population")
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

