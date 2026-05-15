test_that("to_SpatialPoints is deprecated and still works", {
  a <- matrix(0, nrow = 4, ncol = 4)
  a[1,2] <- a[1,3] <- a[2,3] <- a[3,4] <- 1
  a <- a + t(a)
  rownames(a) <- c("Olympia", "St. Louis", "Ames", "Richmond")
  graph <- as.popgraph(a)

  V(graph)$Latitude  <- c(47.15, 38.81, 43.08, 37.74)
  V(graph)$Longitude <- c(-122.89, -89.98, -93.47, -77.16)

  # deprecation warning fires on a valid call
  expect_warning(to_SpatialPoints(graph), "deprecated")

  sp <- suppressWarnings(to_SpatialPoints(graph))
  bbexp <- matrix(c(-122.89, -77.16, 37.74, 47.15), ncol = 2, byrow = TRUE)
  expect_equivalent(bbox(sp), bbexp)
})
