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

test_that("population_graph(decorate = TRUE) adds per-stratum means of numeric columns", {
  data(arapat)
  g0 <- population_graph(arapat)
  expect_setequal(igraph::vertex_attr_names(g0), c("name", "size"))

  x <- arapat
  x$Elevation <- seq_len(nrow(x))
  x$Elevation[x$Population == "101"] <- NA
  g <- population_graph(x, decorate = TRUE)
  expect_setequal(igraph::vertex_attr_names(g),
                  c("name", "size", "Latitude", "Longitude", "Elevation"))
  expect_equal(igraph::E(g)$weight, igraph::E(g0)$weight)
  expect_equal(igraph::V(g)$size, igraph::V(g0)$size)

  nm <- igraph::V(g)$name
  expect_equal(igraph::V(g)$Latitude,
               as.numeric(tapply(x$Latitude, x$Population, mean)[nm]))
  expect_equal(igraph::V(g)$Elevation[nm != "101"],
               as.numeric(tapply(x$Elevation, x$Population, mean)[nm[nm != "101"]]))
  expect_true(is.na(igraph::V(g)$Elevation[nm == "101"]))

  # coordinates match strata_coordinates() and drive the geographic layout
  sc <- strata_coordinates(arapat)
  expect_equal(igraph::V(g)$Longitude, sc$Longitude[match(nm, sc$Stratum)])
  expect_s3_class(plot(g), "ggplot")
})
