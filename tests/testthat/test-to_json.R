context("as_json.R")

test_that("tests", {
  a <- matrix( 0,nrow=4,ncol=4)
  a[1,2] <- a[1,3] <- a[2,3] <- a[1,4] <-1
  a <- a + t(a)
  graph <- as.popgraph(a)
  
  json <- to_json(graph)  
  expect_that( json, is_a("character"))
  expect_that( to_json(FALSE), throws_error() )
  
})