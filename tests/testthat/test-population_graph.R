test_that("population_graph input validation and graph construction", {
  expect_error(population_graph("not_df"))
  expect_error(population_graph(data.frame(x = 1), stratum = "missing"))

  data(arapat)
  # Standard creation
  g <- population_graph(arapat, stratum = "Population")
  expect_true(inherits(g, "popgraph") || inherits(g, "igraph"))
  expect_equal(igraph::vcount(g), length(unique(arapat$Population)))

  # With subsampled loci
  g_sub <- population_graph(arapat, stratum = "Population", numLoci = 3)
  expect_true(inherits(g_sub, "popgraph") || inherits(g_sub, "igraph"))
  expect_equal(igraph::vcount(g_sub), length(unique(arapat$Population)))
})
