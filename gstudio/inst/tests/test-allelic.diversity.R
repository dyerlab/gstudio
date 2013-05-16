context("allelic.diversity.R")

test_that("Allelic Diversity",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  loci <- c(AA,AB,AA,AA,AA,AA,AA,AA,AA,AA,AA)
  
  expect_that( allelic.diversity(loci,"bob"), throws_error() )
  expect_that( allelic.diversity("bob"), throws_error() )
  
  a <- allelic.diversity( loci, mode="A" )
  expect_that( names(a), equals("A"))
  expect_that( a, is_equivalent_to(2) )
  
  ae <- allelic.diversity( loci, mode="Ae" )
  expect_that( ae<1.1, is_true())
  
  a95 <- allelic.diversity( loci, mode="A95" )
  expect_that( a95, is_equivalent_to(1) )
})