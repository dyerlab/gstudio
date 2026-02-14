context("ops_popgraph.R")

test_that("tests", {
  
  e1 <- as.popgraph( igraph::graph_from_atlas(716) )
  e2 <- as.popgraph( igraph::graph_from_atlas(806) )
  
  expect_that( e1 - 2, throws_error() )
  expect_that( e1 - "A", throws_error() )
  expect_that( e1 - FALSE, throws_error() )
  
  e3 <- e1 - e2
  expect_true( inherits(e3,"popgraph"))
  expect_that( length(igraph::V(e3)), equals(7) )
  expect_that( length(igraph::E(e3)), equals(3) )
  
})