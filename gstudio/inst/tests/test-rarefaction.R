context("rarefraction.R")

test_that("Allelic Diversity",{
  
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  loci <- c(AA,AB,AA,AA,AA,AA,AA,AA,AA,AA,AA)
  
  expect_that( rarefaction(loci,size=20,mode="Ae",nperm=99), gives_warning() )
  expect_that( rarefaction(loci), throws_error() )
  expect_that( rarefaction("bob"), throws_error() )
  
})

