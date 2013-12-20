\name{column_class}
\alias{column_class}
\title{Find columns of data in a data.frame for a type of class}
\usage{
  column_class(x, class, mode = c("label", "index")[1])
}
\arguments{
  \item{x}{An object of \code{data.frame} type.}

  \item{class}{The type of class to search for.  If this is
  omitted then the classes of each column will be
  returned.}

  \item{mode}{How you would like the column references to
  be used.  Current values are: \itemize{ \item{label}{The
  name of the column label with the specified class.}
  \item{index}{The numerical index of the column with
  specified class.} }}
}
\value{
  A list of labels or indices indicating where columns of
  the querried type exist or a list of all column classes.
}
\description{
  This convienence function finds the data of a specified
  type in the passed \code{data.frame}.
}
\examples{
locus <- c( locus(1:2), locus(c(1,1)), locus(c(2,2)), locus(2:3) )
pop <- factor( c(rep("A",2), rep("B",2)))
df <- data.frame( Population=pop, X=runif(4), TPI=locus )
df
column_class(df,"factor")
column_class(df,"numeric")
column_class(df, "locus" )
column_class(df,"locus",mode="index")
}
\author{
  Rodney J. Dyer \email{rjdyer@vcu.edu}
}

