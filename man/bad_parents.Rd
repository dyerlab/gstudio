\name{bad_parents}
\alias{bad_parents}
\title{Determines of the indicated parent can possibly be the parent of the offspring}
\usage{
bad_parents(df, AdultID = "ID", OffID = "OffID", verbose = FALSE)
}
\arguments{
  \item{df}{A \code{data.frame} with identification and
  locus columns.}

  \item{AdultID}{The column header for id of the adults.
  All adults have unique identification numbers and all
  offspring have the the same number as the
  maternal/paternal indiviudal from which they were sampled
  or are being compared.  Default = "ID"}

  \item{OffID}{A column indicating offspring identification
  numbers.  By default all adults have OffID=0 (this is how
  I tell if they are really adults and not offspring), and
  all offspring have OffID != 0. Default = "OffID"}

  \item{verbose}{Print out mismatched parent/offspring
  pairs.}
}
\value{
The \code{data.frame} with a new column, Is.Parent with
values of TRUE/FALSE/NA (the NA is for adults).
}
\description{
This function takes a \code{data.frame} full of parents and
offspring and checks to see if the offspring are actually
assigned to a compatible adult.
}

