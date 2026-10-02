test_that("to_geojson input validation", {
  expect_error(to_geojson("not_a_graph"))
  
  g <- igraph::make_graph(~ A - B)
  class(g) <- c("popgraph", "igraph")
  expect_error(to_geojson(g)) # Missing Latitude and Longitude
})

test_that("to_geojson produces valid GeoJSON FeatureCollection", {
  data(lopho)
  data(baja)
  graph <- decorate_graph(lopho, baja)

  json_str <- to_geojson(graph)
  expect_true(is.character(json_str))

  parsed <- jsonlite::fromJSON(json_str)
  expect_equal(parsed$type, "FeatureCollection")
  expect_true(nrow(parsed$features) > 0)
  expect_true("Point" %in% parsed$features$geometry$type)
  expect_true("LineString" %in% parsed$features$geometry$type)

  # Test writing to file
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp))
  to_geojson(graph, file = tmp)
  expect_true(file.exists(tmp))
  expect_true(file.info(tmp)$size > 0)
})
