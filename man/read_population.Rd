\name{read_population}
\alias{read_population}
\title{Read a raw text file in and translate appropriate columns into genotypes}
\usage{
read_population(path, type, locus.columns, phased = FALSE, sep = ",",
  header = TRUE)
}
\arguments{
  \item{path}{The path to the text file}

  \item{type}{An indication of what kind of loci that the
  data represent. The following kinds are recoginzed (n.b.,
  if you have several types load them separately and
  \code{merge} them). \describe{ \item{missing}{The
  default. This will cause \code{read.population()} to read
  each column as a single locus with one allele}
  \item{aflp}{Encoded as 0,1 for absence/presence of
  bands.} \item{column}{Two columns of alleles per locus.}
  \item{separated}{Pre-separated alleles (with ':').}
  \item{snp}{Encoded by the number of minor alleles at the
  locus.} \item{zyme}{Alleles like zymes (e.g., 12 for '1'
  and '2' alleles).} \item{genepop}{Import data that is in
  'genepop' format.} }}

  \item{phased}{A flag indicating the the alleles should
  are of known gametic phase (default=FALSE).}

  \item{sep}{The field separator}

  \item{header}{A logical flag indicating if there is a
  header row (a good idea).}

  \item{locus.columns}{A vector indicating the numerical
  column number for each data type that will be treated as
  a \code{locus} object.}
}
\value{
A \code{data.frame} with \code{locus} columns
pre-formatted.
}
\description{
The function reads in a text file and does the proper
translations for genotypes and spatial coordinates.
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

