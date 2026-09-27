
test_that( "testing",{
  A <- locus( 1:2 )
  B <- locus( c("A","B") )

  expect_that( to_fixed_locus(), throws_error())

  
  fl_A <- to_fixed_locus(A,digits=1)
  fl_B <- to_fixed_locus(B,digits=2)
  
  expect_that( fl_A, is_a("character") )
  expect_that( nchar(fl_A), equals(2) )
  expect_that( nchar(fl_B), equals(4))             
  
  # vectors are formatted element-wise, with missing loci zero-filled
  v <- c( locus(c("1","2")), locus(c("3","4")), locus() )
  expect_equal( to_fixed_locus(v, digits=2), c("0102", "0304", "0000") )
  
})
