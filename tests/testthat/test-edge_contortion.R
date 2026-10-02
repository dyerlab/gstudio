test_that("edge_contortion input validation and attributes", {
  expect_error(edge_contortion(igraph::make_graph(~ A - B))) # missing P
  
  data(lopho)
  data(baja)
  graph <- decorate_graph(lopho, baja)
  
  expect_error(edge_contortion("not_popgraph", P = matrix(0, 5, 5)))
  
  # Mismatched dimensions
  expect_error(edge_contortion(graph, P = matrix(0, 5, 5)))

  # Proper physical distance matrix
  coords <- cbind(igraph::V(graph)$Longitude, igraph::V(graph)$Latitude)
  P <- as.matrix(dist(coords))
  rownames(P) <- colnames(P) <- igraph::V(graph)$name

  g_contort <- edge_contortion(graph, P = P)
  expect_s3_class(g_contort, "popgraph")
  expect_true("contortion" %in% igraph::edge_attr_names(g_contort))
  expect_true("stretch" %in% igraph::edge_attr_names(g_contort))
  expect_true(all(igraph::E(g_contort)$stretch %in% c("Compressed", "Extended")))
})
