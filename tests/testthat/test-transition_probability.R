context("transition_probability.R")

test_that("testing",{
  off  <- locus( 1:2 )
  mom  <- locus( c(1,1) )
  dad1 <- locus( c(2,2) )
  dad2 <- locus( c(1,2) )
  dad3 <- locus( c(1,1) )

  expect_that( transition_probability(), throws_error() ) 
  
  t1 <- transition_probability( off, mom, dad1 )
  t2 <- transition_probability( off, mom, dad2 )
  t3 <- transition_probability( off, mom, dad3 )
  
  expect_that( t1, equals(1.0) )
  expect_that( t2, equals(0.5) )
  expect_that( t3, equals(0.0) )

  
  t4 <- transition_probability( mom, dad3)  # 1.0
  t5 <- transition_probability( off, mom )  # 0.5
  t6 <- transition_probability( mom, dad1 ) # 0.0
  expect_that( t4, equals(1.0))
  expect_that( t5, equals(0.5))
  expect_that( t6, equals(0.0))
  
  
  # test to see if it handles 
  df <- data.frame( ID=1:5, PGM=c(off,mom,dad1,dad2,dad3))
  expect_that( transition_probability( df[1,], df[2,] ), is_equivalent_to(0.5))
  expect_that( transition_probability( df[2,], df[3,] ), is_equivalent_to(0.0))
  expect_that( transition_probability( df[2,], df[5,] ), is_equivalent_to(1.0))
  
  df$PGI <- df$PGM
  df$TPI <- df$PGM
  expect_that( transition_probability( df[1,], df[2,], df[4,] ), is_equivalent_to(0.5^3))
  expect_that( transition_probability( df[1,], df[2,], df[4,], multilocus=FALSE ), is_equivalent_to(c(0.5,0.5,0.5)))
  expect_that( transition_probability( df[1,], df[2,], df[3,] ), is_equivalent_to(1.0))
  expect_that( transition_probability( df[1,], df[2,], df[5,] ), is_equivalent_to(0.0))
  
  

  df$PGI[1] <- NA
  expect_that( transition_probability( df[1,], df[2,], df[4,], multilocus=FALSE ), is_equivalent_to( c(0.5,NA,0.5)) )
  expect_that( transition_probability( df[1,], df[2,], df[4,] ), is_equivalent_to( 0.25) )
  
               
})