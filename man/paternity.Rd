\name{paternity}
\alias{paternity}
\title{Estimates fractional paternity probability}
\usage{
  paternity(offspring, mother, fathers, ID = "ID",
    OffID = "OffID")
}
\arguments{
  \item{offspring}{A particular offspring. Often as a row
  from a \code{data.frame} with columns as loci and other
  meta data.}

  \item{mother}{The assumed mother of the offspring as a
  row from a \code{data.frame} with columns as loci and
  other meta data.}

  \item{fathers}{A \code{data.frame} of potential fathers.}

  \item{ID}{The name of the column where the dad's adult ID
  column is found in the \code{data.frame} of potential
  fathers.}

  \item{OffID}{The name of the column where the offspring
  ID is located.  All offspring from a maternal individual
  should have have the same maternal \code{ID} to indicate
  which mother they are from but must also have a unique
  offspring ID.}
}
\value{
  A \code{data.frame} with indications of paternity by row.
  Columns will include ID, OffID, DadID, and potentially
  Fij.
}
\description{
  This function estimates fractional paternity for
  offspring given a set of potential fathers.
}
\examples{
freqs <- c(0.55, 0.30, 0.15, 0.34, 0.34, 0.32)
loci <- c(rep("TPI",3), rep("PGM",3))
alleles <- c(LETTERS[1:3],LETTERS[8:10])
f <- data.frame(Locus=loci, Allele=alleles, Frequency=freqs)
adults <- make_population(f,N=20)
adults
offs <- mate( adults[1,], adults[2,], N=10)
offs
paternity( offs, adults[1,], adults )
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

