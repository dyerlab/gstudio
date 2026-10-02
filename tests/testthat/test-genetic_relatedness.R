

test_that("error checks",{

  
  expect_that( genetic_relatedness( data.frame(X=1)), throws_error() )
  expect_that( genetic_relatedness( numeric(10), mode="Ritland"), throws_error() )
  
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  AD <- locus( c("A","D") )
  BC <- locus( c("B","C") )
  BD <- locus( c("B","D") )
  CC <- locus( c("C","C") )
  CD <- locus( c("C","D") )
  DD <- locus( c("D","D") )
  loci <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD) 
  
  expect_that( genetic_relatedness( loci, mode="Bob"), throws_error() )
  df <- data.frame( ID=1:10 )
  expect_that( genetic_relatedness(df),throws_error() )
  
  df$TPI <- loci
  r <- genetic_relatedness( df, mode="Nason")
  expect_that( r, is_a("matrix") )
  expect_that( dim(r), is_equivalent_to(c(10,10)))
  expect_that( sum(diag(r)), is_equivalent_to(10))

  # Test LynchRitland
  r_lynch <- genetic_relatedness( df, mode="LynchRitland" )
  expect_true( is(r_lynch, "matrix") )
  expect_equal( dim(r_lynch), c(10, 10) )
  expect_equal( diag(r_lynch), rep(1, 10) )
  # Off-diagonal values are computed (not all zero)
  expect_true( any(r_lynch[upper.tri(r_lynch)] != 0) )
  expect_equal( r_lynch, t(r_lynch) )

  # Test Ritland
  r_ritland <- genetic_relatedness( df, mode="Ritland" )
  expect_true( is(r_ritland, "matrix") )
  expect_equal( dim(r_ritland), c(10, 10) )
  expect_equal( diag(r_ritland), rep(1, 10) )
  expect_true( any(r_ritland[upper.tri(r_ritland)] != 0) )
  expect_equal( r_ritland, t(r_ritland) )

  # Test Queller
  r_queller <- genetic_relatedness( df, mode="Queller" )
  expect_true( is(r_queller, "matrix") )
  expect_equal( dim(r_queller), c(10, 10) )
  expect_equal( diag(r_queller), rep(1, 10) )
  expect_true( any(r_queller[upper.tri(r_queller)] != 0) )
  expect_equal( r_queller, t(r_queller) )
})


