test_that("to_df input validation and conversion", {
  expect_error(to_df("not_a_graph"))
  
  data(lopho)
  expect_error(to_df(lopho, mode = "unsupported"))

  df_nodes <- to_df(lopho, mode = "nodes")
  expect_s3_class(df_nodes, "data.frame")
  expect_true(nrow(df_nodes) == igraph::vcount(lopho))
  expect_equal(names(df_nodes)[1], "Stratum")
  expect_setequal(df_nodes$Stratum, igraph::V(lopho)$name)

  df_edges <- to_df(lopho, mode = "edges")
  expect_s3_class(df_edges, "data.frame")
  expect_true(nrow(df_edges) == igraph::ecount(lopho))
  expect_equal(names(df_edges)[1:2], c("from", "to"))
})
