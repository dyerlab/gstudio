context("read_popgraph.R")

test_that("testing", {

  file <- system.file("extdata","lopho.pgraph",package="gstudio")
  if( file == "")
    file <- "/Users/rodney/Documents/Software/Packages/gstudio/inst/extdata/lopho.pgraph"
  
  require(igraph)
  graph <- read_popgraph( file )
  
  
  expect_that( graph, is_a("igraph") )
  expect_that( length( V(graph) ), equals(21) )
  expect_that( length( E(graph) ), equals(50) )
  expect_true( is_weighted(graph))
  
}
)
