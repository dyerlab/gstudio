test_that("to_df input validation and conversion", {
  expect_error(to_df("not_a_graph"))
  
  data(lopho)
  expect_error(to_df(lopho, mode = "unsupported"))

  df_nodes <- to_df(lopho, mode = "nodes")
  expect_s3_class(df_nodes, "data.frame")
  expect_true(nrow(df_nodes) == igraph::vcount(lopho))
  expect_true("name" %in% names(df_nodes))

  df_edges <- to_df(lopho, mode = "edges")
  expect_s3_class(df_edges, "data.frame")
  expect_true(nrow(df_edges) == igraph::ecount(lopho))
  expect_true(all(c("From", "To") %in% names(df_edges)))
})
