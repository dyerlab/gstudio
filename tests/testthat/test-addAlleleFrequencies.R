context("addAlleleFrequencies")

test_that("errors on non-existent locus", {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))
  df <- data.frame(
    Population = c("Pop1", "Pop1", "Pop2", "Pop2"),
    Longitude = c(-77.5, -77.5, -78.0, -78.0),
    Latitude = c(37.5, 37.5, 38.0, 38.0),
    TPI = c(AA, AB, AB, BB)
  )
  expect_error(
    addAlleleFrequencies(df, locus = "FAKE"),
    "Locus 'FAKE' not found"
  )
})

test_that("wide-matrix data transformation is correct", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaflet.minicharts")

  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))
  df <- data.frame(
    Population = c("Pop1", "Pop1", "Pop2", "Pop2"),
    Longitude = c(-77.5, -77.5, -78.0, -78.0),
    Latitude = c(37.5, 37.5, 38.0, 38.0),
    TPI = c(AA, AB, AB, BB)
  )

  freqs <- frequencies(df, loci = "TPI", stratum = "Population")
  wide <- reshape2::dcast(freqs, Stratum ~ Allele,
                          value.var = "Frequency", fill = 0)

  expect_true("A" %in% names(wide))
  expect_true("B" %in% names(wide))
  expect_equal(nrow(wide), 2)

  # Pop1 has AA + AB => A freq = 0.75, B freq = 0.25
  pop1 <- wide[wide$Stratum == "Pop1", ]
  expect_equal(pop1$A, 0.75)
  expect_equal(pop1$B, 0.25)

  # Pop2 has AB + BB => A freq = 0.25, B freq = 0.75
  pop2 <- wide[wide$Stratum == "Pop2", ]
  expect_equal(pop2$A, 0.25)
  expect_equal(pop2$B, 0.75)
})

test_that("returns a leaflet htmlwidget", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaflet.minicharts")

  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))
  df <- data.frame(
    Population = c("Pop1", "Pop1", "Pop2", "Pop2"),
    Longitude = c(-77.5, -77.5, -78.0, -78.0),
    Latitude = c(37.5, 37.5, 38.0, 38.0),
    TPI = c(AA, AB, AB, BB)
  )

  result <- addAlleleFrequencies(df, locus = "TPI")
  expect_true(inherits(result, "leaflet"))
  expect_true(inherits(result, "htmlwidget"))
})

test_that("works with existing map argument", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaflet.minicharts")

  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))
  df <- data.frame(
    Population = c("Pop1", "Pop1", "Pop2", "Pop2"),
    Longitude = c(-77.5, -77.5, -78.0, -78.0),
    Latitude = c(37.5, 37.5, 38.0, 38.0),
    TPI = c(AA, AB, AB, BB)
  )

  existing_map <- leaflet::leaflet() |> leaflet::addTiles()
  result <- addAlleleFrequencies(df, locus = "TPI", map = existing_map)
  expect_true(inherits(result, "leaflet"))
  expect_true(inherits(result, "htmlwidget"))
})
