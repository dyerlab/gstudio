context("to_fixed_locus.R")

test_that( "testing",{
  A <- locus( 1:2 )
  B <- locus( c("A","B") )

  expect_that( to_fixed_locus(), throws_error())
  expect_that( to_fixed_locus(A), throws_error() )
  
  fl_A <- to_fixed_locus(A,digits=1)
  fl_B <- to_fixed_locus(B,digits=2)
  
  expect_that( fl_A, is_a("character") )
  expect_that( nchar(fl_A), equals(2) )
  expect_that( nchar(fl_B), equals(4))             
  
})
