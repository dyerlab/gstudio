context("plot.R")


test_that("locus",{
  
  loci <- c( locus(1:2), locus(1:2), locus(1:2) )
  p <- plot.locus( loci )
  
  expect_true( inherits(p,"ggplot"))
  expect_that( p$data, is_a("data.frame"))
  expect_true( all(p$data$Frequency == 0.5) )
})





test_that("data.frame",{
  
  expect_that( 1, equals(1) )
})

