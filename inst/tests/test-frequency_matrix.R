context("frequency_matrix.R")

test_that("frequency matrix", {
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
  loc1 <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD)
  loc2 <- c(AA,AA,AC,AA,CC,CC,AC,CC,AA,AC)
  
  df <- data.frame( Population=c(rep("A",5),rep("B",5)), TPI=loc1, PGM=loc2 )
  
  expect_that( frequency_matrix(), throws_error())
  expect_that( frequency_matrix(df,stratum="bob"), throws_error())
  expect_that( frequency_matrix(df,stratum="Population",locus="PGI"), throws_error())
  
  f <- frequency_matrix(df, stratum="Population", locus="PGM")
  expect_that( f, is_a("data.frame"))
  expect_that( dim(f), is_equivalent_to(c(2,3)))
  expect_that( f[1,2] + f[1,3] == 1, is_true())
  expect_that( f[2,2] + f[2,3] == 1, is_true())
  
  f <- frequency_matrix(df, stratum="Population", locus="TPI")
  expect_that( f, is_a("data.frame"))
  expect_that( dim(f), is_equivalent_to(c(2,5)))
  expect_that( f[1,2] + f[1,3] + f[1,4] + f[1,5], is_equivalent_to(1.0))

  
})
