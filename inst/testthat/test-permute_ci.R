context("permute_ci.R")

test_that("catching errors", {
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  loci <- c(AA,AB,AA,AA,AA,AA,AA,AA,AA,AA,AA)
  
  expect_that( permute_ci(), throws_error() )
  expect_that( permute_ci(x="bob", FUN=He), throws_error() )
  expect_that( permute_ci(loci, stratum=1:3, FUN=He), throws_error() )
  expect_that( permute_ci(c(loci,NA), FUN=Fis, nperm=0), gives_warning() )
  
  f.null <- permute_ci( loci, FUN=Fis, nperm=9)
  expect_that( f.null, is_a("numeric"))
  expect_that( length(f.null), equals(9))
  
  
})
  