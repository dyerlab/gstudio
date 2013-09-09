context("geom_strata.R")

test_that("shortest paths", {

  data <- data.frame(Stratum=c("A","B"), Longitude=c(-77.4548,-77.4547), Latitude=c(37.5460,37.5459) )
  p <- geom_strata( aes(x=Longitude,y=Latitude,stratum=Stratum), data=data)
  expect_that( p, is_a("proto"))
  expect_that( p, is_a("environment"))
  
})