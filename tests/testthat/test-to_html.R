context("to_html.R")

test_that("tests", {
  a <- matrix( 0,nrow=4,ncol=4)
  a[1,2] <- a[1,3] <- a[2,3] <- a[1,4] <-1
  a <- a + t(a)
  graph <- as.popgraph( a )
  
  expect_that( to_html(FALSE), throws_error() )
  
  ret <- to_html(graph)
  expect_that(ret,is_a("character"))

})