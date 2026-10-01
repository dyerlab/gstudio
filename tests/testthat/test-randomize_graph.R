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

test_that("degree mode preserves degrees and stays simple on dense graphs", {
  set.seed(42)
  g <- igraph::sample_gnm(25, 150)
  igraph::V(g)$name <- paste0("P", 1:25)
  igraph::E(g)$weight <- runif(150)

  for (i in 1:20) {
    r <- randomize_graph(g, "degree")
    expect_true(igraph::is_simple(r))
    expect_equal(igraph::degree(r)[igraph::V(g)$name], igraph::degree(g))
  }
  expect_null(igraph::E(r)$weight)
})
