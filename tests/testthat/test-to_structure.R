context("to_structure.R")

test_that("text output", {
  A <- locus( c("1","1"))
  B <- locus( c("1","2"))
  C <- locus( c("2","2"))
  loc1 <- c( A, A, B, B, B, B, C, C)
  loc2 <- c( A, B, B, C, A, B, A, C)

  pop <- data.frame( Population=c( rep("A",4),rep("B",4) ), loc1, loc2)
  
  expect_that( to_structure(), throws_error() )
  
  sp <- to_structure(pop, stratum="Population")
  expect_that( sp, is_a("character"))
  expect_that( nchar(sp), equals(127) )
  
})

