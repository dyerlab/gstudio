context("randomize_graph.R")

test_that("testing", {
  
  expect_that( randomize_graph(), throws_error())
  
  x <- sample( c( rep(1,20), rep(0,25)), size=45, replace=FALSE )
  A <- matrix(0,nrow=10,ncol=10)
  A[ lower.tri(A)] <- x
  A <- A + t(A)
  g <- igraph::graph_from_adjacency_matrix(A,mode="undirected")
  
  expect_that( randomize_graph(g, "bob"), throws_error() )
  
  g1 <- randomize_graph(g,"full")
  g2 <- randomize_graph(g,"degree")
  
      
}
)
