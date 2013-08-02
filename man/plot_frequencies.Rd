\name{plot_frequencies}
\alias{plot_frequencies}
\title{Plot allele frequencies}
\usage{
  plot_frequencies(x)
}
\arguments{
  \item{x}{The data frame with allele frequencies from
  \code{frequencies}.}
}
\value{
  An object of type \code{ggplot}
}
\description{
  This function allows you to plot allele frequencies for
  several potential levels of stratum and loci.
}
\note{
  This function does a pretty good job of plotting but if
  you have too many loci and/or strata, you may have a
  difficult time getting it to look right.  By default, the
  function does NOTHING to prevent you from making crappy
  plots with too many levels.  To plot a reduced number of
  loci/strata, use a subset of your data.
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

