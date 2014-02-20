\name{mate}
\alias{mate}
\title{Produces offspring}
\usage{
mate(mom, dad, N = 1)
}
\arguments{
  \item{mom}{This is the 'maternal' individual in that all
  the metadata in the \code{data.frame} that describes this
  individual will be transfered to the offspring.}

  \item{dad}{This is the 'paternal' individual and will
  contribute only half of its genetic compliement to the
  offspring.}

  \item{N}{The number of offspring to produce.}
}
\value{
A \code{data.frame} of offspring.
}
\description{
This function is mostly for simulation work and functions
to produce a set of offpsring for the individuals passed.
}
\note{
There are several 'hidden' things in this routine.  First,
if you do not pass a 'dad' object, it will assume you want
a selfed offspring.  Second, it will by default only make a
single offspring.  Next, if you have a column labelled
"Sex" it will make a random selection of which sex each
offspring should be and only mate the opposite sexes based
upon the levels of the values in the Sex column.  Finally,
if there are columns ID and OffID in the mom, then all
offspring will have the same ID as the mom but will have
OffID equal to 1:N to conform with how the functions like
\code{paternity()} operate.  If you do not have ID and
OffID then it will do nothing special.
}
\examples{
f <- data.frame(Allele=LETTERS[1:2], Frequency=c(0.5,0.5))
adults <- make_population(f,N=2)
adults
mate( adults[1,], adults[2,], N=10)
}
\author{
Rodney J. Dyer \email{rjdyer@vcu.edu}
}

