context("plot.R")


test_that("locus",{
  loci <- c( locus(1:2), locus(1:2), locus(1:2) )
  p <- plot.locus( loci )
  
  expect_that( inherits(p,"ggplot"), is_true() )
  expect_that( p$data, is_a("data.frame"))
  expect_that( all(p$data$Frequency == 0.5), is_true() )
})




test_that("genetic_distance",{
  require(ggplot2)
  x <- abs(matrix(rnorm(16),ncol=4))
  x <- x + t(x)
  diag(x) <- 0
  class( x ) <- c("genetic_distance","matrix")
  p <- plot.genetic_distance( x ) 
  expect_that( inherits(p,"ggplot"), is_true() )
  expect_that( p$data, is_a("data.frame"))
  expect_that( p$data$Values, is_a("numeric"))
  
  
})



test_that("data.frame",{
  
  expect_that( 1, equals(1) )
})

