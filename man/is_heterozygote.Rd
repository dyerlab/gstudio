\name{is_heterozygote}
\alias{is_heterozygote}
\title{Determine heterozygosity}
\usage{
  is_heterozygote(x)
}
\arguments{
  \item{x}{A \code{code} object (single or vector)}
}
\value{
  A logical flag indicating that there are at least two
  kinds of alleles present in the \code{locus} object.
}
\description{
  This function will determine the heterozygosity of one or
  more objects of type \code{locus}.
}
\note{
  A haploid or NA locus is never heterozygous.
}
\examples{
loc <- locus( 1:2 )
is_heterozygote( loc )
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

