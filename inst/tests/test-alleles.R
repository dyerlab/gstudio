context("alleles.R")

test_that("testing", {

  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  ZZ <- locus()
  loci <- c(AA,AB,ZZ)
  

  expect_that( alleles(AA), is_equivalent_to(c("A","A")) )  
  expect_that( alleles(AB), is_equivalent_to(c("A","B")) )
  expect_that( alleles(ZZ), is_equivalent_to( character(0) ) )
  expect_that( alleles( loci ), is_equivalent_to( matrix(c("A","A",NA,"A","B",NA),ncol=2) ) )

  loci <- c(ZZ,ZZ,ZZ)
  a <- alleles( loci )
  expect_that( length(a), equals(0))

  
}
)

