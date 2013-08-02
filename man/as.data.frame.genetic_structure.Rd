\name{as.data.frame.genetic_structure}
\alias{as.data.frame.genetic_structure}
\title{Conversion of genetic_structure to data.frame}
\usage{
  \method{as.data.frame}{genetic_structure} (x, ...)
}
\arguments{
  \item{x}{An objet of type
  \code{\link{genetic_structure}}}

  \item{...}{Ignored}
}
\value{
  A data frame
}
\description{
  Converts the \code{\link{genetic_structure}} object to a
  common data.frame object with keys 'locus', 'Allele' and
  'Frequency'
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

