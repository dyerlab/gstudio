test_that("Ritland relatedness",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  BC <- locus( c("B","C") )
  CC <- locus( c("C","C") )
  x <- c(AA,AA,AB,BB,CC,AB,AC,BB,BC,CC)
  
  expect_error( rel_ritland() )
  expect_error( rel_ritland("B") )
  
  f <- rel_ritland( x )
  expect_true( is(f, "matrix") )
  expect_equal( dim(f), c(10, 10) )
  expect_true( is.na(f[1, 1]) )
  # Symmetry
  expect_equal( f[1, 2], f[2, 1] )
  expect_equal( f[1, 3], f[3, 1] )
  # Identical genotypes (inds 1 and 2) should have higher relatedness than disjoint (inds 1 and 4)
  expect_true( f[1, 2] > f[1, 4] )

  # Multilocus test
  df <- data.frame(L1 = x, L2 = rev(x))
  f_multi <- rel_ritland( df )
  expect_equal( dim(f_multi), c(10, 10) )
  expect_equal( f_multi, t(f_multi) )
})
