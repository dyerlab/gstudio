context("geom_locus.R")

test_that("making the locus object", {

  data(arapat)
  p <- geom_locus( aes(x=WNT), data=arapat )
  expect_that( p, is_a("ggproto"))
  expect_that( p, is_a("LayerInstance"))
  expect_that( p, is_a("Layer"))
  
  
  p <- geom_locus( aes(x=WNT, fill=Cluster), data=arapat )
  expect_that( p, is_a("ggproto"))
  expect_that( p, is_a("LayerInstance"))
  expect_that( p, is_a("Layer"))
  
})