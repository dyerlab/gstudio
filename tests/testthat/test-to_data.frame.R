test_that("to_data.frame conversion for nodes and edges", {
  expect_error(to_data.frame("not_a_graph"))

  data(lopho)
  expect_error(to_data.frame(lopho, mode = "invalid"))

  nodes_df <- to_data.frame(lopho, mode = "nodes")
  expect_s3_class(nodes_df, "data.frame")
  expect_equal(nrow(nodes_df), igraph::vcount(lopho))

  edges_df <- to_data.frame(lopho, mode = "edges")
  expect_s3_class(edges_df, "data.frame")
  expect_equal(nrow(edges_df), igraph::ecount(lopho))
  expect_true(all(c("source", "target", "value") %in% names(edges_df)))
})
