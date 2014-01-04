context("to_genepop.R")

test_that("text output", {
  A <- locus( c("1","1"))
  B <- locus( c("1","2"))
  C <- locus( c("2","2"))
  loc1 <- c( A, A, B, B, B, B, C, C)
  loc2 <- c( A, B, B, C, A, B, A, C)

  pop <- data.frame( Population=c( rep("A",4),rep("B",4) ), loc1, loc2)
  
  expect_that( to_genepop(), throws_error() )
  
  gp <- to_genepop(pop, stratum="Population")
  expect_that( gp, is_a("character"))
  expect_that( nchar(gp), equals(135) )
  
})

