context("dist_nei.R")


test_that("test",{

  f <- matrix( c(0.1, 0.3, 0.6, 0.2, 0.4, 0.4), ncol=3, byrow=T )
  idx <- c(1,2)
  
  d <- dist.nei( idx, f)
  
  top <- sum( f[1,] * f[2,] )
  bot <- sqrt( sum( f[1,]^2) * sum(f[2,]^2) )

  n <- -log(top/bot)
  expect_that( d, equals(n) )
  

})