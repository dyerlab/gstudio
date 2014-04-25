context("dist_amova.R adult")


test_that("strata",{
  
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
  D <- dist_amova( loci )
  
  expect_that( D[1,1], equals(0) )
  expect_that( D[1,2], equals(1) )
  expect_that( D[1,3], equals(1) )
  expect_that( D[1,4], equals(1) )
  expect_that( D[1,5], equals(4) )
  expect_that( D[1,6], equals(3) )
  expect_that( D[1,7], equals(3) )
  expect_that( D[1,8], equals(4) )
  expect_that( D[1,9], equals(3) )
  expect_that( D[1,10], equals(4) )
  expect_that( D[2,9], equals(2) ) 

})


context("dist_amova.R 2Gener")
test_that("strata",{
  loci <- c( locus("1"),
             locus("3"),
             locus("3"),
             locus(c("1","3")),
             locus(c("2","4")),
             locus("2"),
             locus("4"))
  
  D <- dist_amova(loci)
  
  expect_that( is(D,"matrix"), is_true())
  expect_that( dim(D), is_equivalent_to(c(7,7)))
  expect_that( diag(D), is_equivalent_to(rep(0,7)))
  expect_that( D[1,2], is_equivalent_to(1.0))
  
  # 1/3 vs 3
  expect_that( D[3,4], is_equivalent_to(0.16))  
  
  # 1/3 vs 2
  expect_that( D[4,6], is_equivalent_to(0.76))
  
  # 1/3 2/4
  expect_that( D[4,5], is_equivalent_to(0.51))
  
  
  
})

