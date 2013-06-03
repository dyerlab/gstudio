context("allelic_diversity.R")

test_that("Allelic Diversity",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  loci <- c(AA,AB,AA,AA,AA,AA,AA,AA,AA,AA,AA)
  
  expect_that( allelic_diversity(loci,"bob"), throws_error() )
  expect_that( allelic_diversity("bob"), throws_error() )
  
  a <- allelic_diversity( loci, mode="A" )
  expect_that( names(a), equals("A"))
  
})