context("genotype_counts.R")

test_that("testing",{
  Locus1 <- c( locus( c(1,1) ),
               locus( c(2,2) ),
               locus( c(2,2) ),
               locus( c(1,2) ),
               locus( c(1,1) ),
               locus( c(2,2) ),
               locus( c(1,2) ),
               locus( c(2,1) ),
               locus( c(2,1) ),
               locus( c(2,2) ),
               locus( c(2,3) ))
  Locus2 <- c( locus( c(1,1) ),
               locus( c(2,2) ),
               locus( c(2,2) ),
               locus( c(1,2) ),
               NA,
               locus( c(2,2) ),
               locus( c(1,2) ),
               locus( c(2,1) ),
               locus( c(2,1) ),
               NA,
               locus( c(2,3) ))
  Pop <- c( rep("First",5),rep("Second",6))
  df <- data.frame( Population=Pop, Locus1, Locus2 )
  
  expect_that( genotype_counts(), throws_error() )
  expect_that( genotype_counts(df,"Bob"), throws_error())
  
  f <- genotype_counts(df)
  expect_that( f, is_a("data.frame"))
  expect_that( dim(f), is_equivalent_to(c(1,4)))
  expect_that( names(f), is_equivalent_to(c("Stratum","N","Locus1","Locus2")))  
  expect_that( f$N[1], is_equivalent_to(11))
  expect_that( f$Locus1[1], is_equivalent_to(11))  
  expect_that( f$Locus2[1], is_equivalent_to(9))
  
  f <- genotype_counts( df, stratum="Population")
  expect_that( dim(f), is_equivalent_to(c(2,4)))
  expect_that( f$N, is_equivalent_to(c(5,6)))
  expect_that( f$Locus1, is_equivalent_to(c(5,6)))  
  expect_that( f$Locus2, is_equivalent_to(c(4,5)))
})