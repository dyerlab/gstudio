\name{to_genepop}
\alias{to_genepop}
\title{Translates a \code{data.frame}}
\usage{
to_genepop(df, stratum = "Population")
}
\arguments{
  \item{df}{The \code{data.frame} to be written to the
  output.}

  \item{stratum}{The stratum to use as "POP"
  (default="Population")}
}
\value{
A string representation of the \code{data.frame} formatted
as
}
\description{
This is a basic output file formatter for saving files
necessary for saving output in textual formats.
}
\examples{
A <- locus( c("1","1"))
B <- locus( c("1","2"))
C <- locus( c("2","2"))
loc1 <- c( A, A, B, B, B, B, C, C)
loc2 <- c( A, B, B, C, A, B, A, C)
pop <- data.frame( Population=c( rep("A",4),rep("B",4) ), loc1, loc2)
gp <- to_genepop( pop )
cat(gp)
}
\author{
Rodney J. Dyer <rjdyer@vcu.edu>
}

