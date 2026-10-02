
test_that("testing", {

  file <- system.file("extdata","lopho.pgraph",package="gstudio")
  if( file == "" || !file.exists(file) )
    file <- file.path("..", "..", "inst", "extdata", "lopho.pgraph")
  
  suppressWarnings(require(igraph, quietly = TRUE))
  graph <- read_popgraph( file )
  
  
  expect_that( graph, is_a("igraph") )
  expect_that( length( V(graph) ), equals(21) )
  expect_that( length( E(graph) ), equals(50) )
  expect_true( is_weighted(graph))
  
}
)
