\name{summary.locus}
\alias{summary.locus}
\title{Provides a summary of the \code{locus} object}
\usage{
\method{summary}{locus}(object, ...)
}
\arguments{
  \item{object}{The \code{locus} object to summarize
  (typically a vector).}

  \item{\dots}{Ignored}
}
\value{
A summary of the vector of \code{locus} objects in
\code{object}
}
\description{
Provides an overload of the \code{summary} object so that
when you put this into a \code{data.frame} and ask for a
summary, it will provide you a summary of genotype counts.
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

