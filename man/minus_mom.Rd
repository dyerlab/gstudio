% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/minus_mom.R
\name{minus_mom}
\alias{minus_mom}
\title{Subtracts maternal component to offspring genotypes}
\usage{
minus_mom(x, MomCol = "ID", OffCol = "OffID", Check = FALSE)
}
\arguments{
\item{x}{A \code{data.frame} with mother and offspring \code{locus} columns.}

\item{MomCol}{The name of the column indicating maternal ID.  All offspring
from a mother MUST have the same MomCol value.}

\item{OffCol}{The name of the column indicating the offspring ID number.  It is
required that maternal individuals have OffCol="0" indicating that this is the 
mother of those offspring.}

\item{Check}{A flag (default \code{FALSE}) that will run through the reduction
process and produce a check of all mom/off/locus problems that are observed.}
}
\value{
Either a \code{data.frame} object of only the offspring after removing the
 contribution of each maternal individual or a data.frame with
}
\description{
This function removes the female component to the offspring
 genotypes. It is one step in the \code{2gener} analysis. The
 coding of the \code{MomCol} and the \code{OffCol} headers 
 are specific so that this algorithm can differentiate between
 maternal individuals and offspring.
}
\examples{
AA <- locus( c("A","A") )
AB <- locus( c("A","B") )
BB <- locus( c("B","B") )
AC <- locus( c("A","C") )
AD <- locus( c("A","D") )
BC <- locus( c("B","C") )
BD <- locus( c("B","D") )
CC <- locus( c("C","C") )
CD <- locus( c("C","D") )
DD <- locus( c("D","D") ) 
EE <- locus( c("E","E") )
F  <- locus( c("F"))
G  <- locus( )
loci <- c(AA,AB,AC,AD,BB,BC,BD,CD,CD,DD) 
offID <- c(0 ,1, 2, 3, 0, 1, 2, 1, 0, 2 )
momID <- c(rep("A",4), rep("B",3), rep("C",3))
df <- data.frame( ID=factor(momID), OffID=factor(offID), TPI=loci )
minus_mom( df )
loci <- c(AA,AB,BB,AD,BB,BC,G,CD,EE,F) 
df <- data.frame( ID=factor(momID), OffID=factor(offID), TPI=loci )
minus_mom( df, Check=TRUE )
}
\author{
Rodney J. Dyer <rjdyer@vcu.edu>
}
