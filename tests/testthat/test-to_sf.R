test_that("to_sf input validation", {
  g <- igraph::make_graph(~ A - B)
  expect_error(to_sf(g, what = "invalid"))
  expect_error(to_sf(g, what = "nodes")) # Missing coordinates
})

test_that("to_sf produces sf POINT and LINESTRING geometries", {
  data(lopho)
  data(baja)
  graph <- decorate_graph(lopho, baja)

  nodes_sf <- to_sf(graph, what = "nodes")
  expect_s3_class(nodes_sf, "sf")
  expect_true(all(sf::st_geometry_type(nodes_sf) == "POINT"))
  expect_equal(sf::st_crs(nodes_sf)$epsg, 4326)

  edges_sf <- to_sf(graph, what = "edges")
  expect_s3_class(edges_sf, "sf")
  expect_true(all(sf::st_geometry_type(edges_sf) == "LINESTRING"))
  expect_equal(sf::st_crs(edges_sf)$epsg, 4326)
})
