context("addAlleleFrequencies")

# ---- shared test data -------------------------------------------------------

make_test_data <- function() {
  AA <- locus(c("A", "A"))
  AB <- locus(c("A", "B"))
  BB <- locus(c("B", "B"))
  data.frame(
    Population = c("Pop1", "Pop1", "Pop2", "Pop2"),
    Longitude  = c(-77.5, -77.5, -78.0, -78.0),
    Latitude   = c(37.5, 37.5, 38.0, 38.0),
    TPI        = c(AA, AB, AB, BB)
  )
}

make_map <- function(df) {
  coords <- strata_coordinates(df, stratum = "Population")
  leaflet::leaflet(data = coords) |> leaflet::addTiles()
}

# ---- tests ------------------------------------------------------------------

test_that("errors when map has no embedded coordinate data", {
  skip_if_not_installed("leaflet")
  df    <- make_test_data()
  freqs <- frequencies(df, loci = "TPI", stratum = "Population")
  bare_map <- leaflet::leaflet() |> leaflet::addTiles()

  expect_error(
    addAlleleFrequencies(bare_map, freqs),
    "No coordinate data found"
  )
})

test_that("errors when coord and freq strata have no overlap", {
  skip_if_not_installed("leaflet")
  df    <- make_test_data()
  map   <- make_map(df)  # strata: Pop1, Pop2

  # freqs computed under different stratum names
  df2 <- df; df2$Population <- paste0("X", df2$Population)
  freqs_bad <- frequencies(df2, loci = "TPI", stratum = "Population")  # XPop1, XPop2

  expect_error(
    addAlleleFrequencies(map, freqs_bad),
    "No shared strata"
  )
})

test_that("warns when some coord strata have no matching frequencies", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaflet.minicharts")
  df    <- make_test_data()
  map   <- make_map(df)  # strata: Pop1, Pop2

  # freqs only for Pop1
  freqs_partial <- frequencies(df[df$Population == "Pop1", ], loci = "TPI",
                               stratum = "Population")

  expect_warning(
    addAlleleFrequencies(map, freqs_partial),
    "coord strata have no matching frequencies"
  )
})

test_that("errors on non-existent locus", {
  skip_if_not_installed("leaflet")
  df    <- make_test_data()
  freqs <- frequencies(df, loci = "TPI", stratum = "Population")
  map   <- make_map(df)

  expect_error(
    addAlleleFrequencies(map, freqs, locus = "FAKE"),
    "Locus 'FAKE' not found"
  )
})

test_that("uses single locus silently when locus= is NULL", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaflet.minicharts")
  df    <- make_test_data()
  freqs <- frequencies(df, loci = "TPI", stratum = "Population")
  map   <- make_map(df)

  expect_no_warning(addAlleleFrequencies(map, freqs))
})

test_that("warns and uses first locus when multiple loci present", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaflet.minicharts")

  AA <- locus(c("A", "A")); AB <- locus(c("A", "B")); BB <- locus(c("B", "B"))
  df <- data.frame(
    Population = c("Pop1", "Pop1", "Pop2", "Pop2"),
    Longitude  = c(-77.5, -77.5, -78.0, -78.0),
    Latitude   = c(37.5, 37.5, 38.0, 38.0),
    TPI        = c(AA, AB, AB, BB),
    EN         = c(AA, BB, AA, AB)
  )
  freqs <- frequencies(df, loci = c("TPI", "EN"), stratum = "Population")
  map   <- make_map(df)

  expect_warning(
    addAlleleFrequencies(map, freqs),
    "Multiple loci"
  )
})

test_that("wide-matrix frequencies are correct", {
  df    <- make_test_data()
  freqs <- frequencies(df, loci = "TPI", stratum = "Population")
  sub   <- freqs[freqs$Locus == "TPI", ]
  wide  <- reshape2::dcast(sub, Stratum ~ Allele, value.var = "Frequency", fill = 0)

  # Pop1: AA + AB => A = 0.75, B = 0.25
  pop1 <- wide[wide$Stratum == "Pop1", ]
  expect_equal(pop1$A, 0.75)
  expect_equal(pop1$B, 0.25)

  # Pop2: AB + BB => A = 0.25, B = 0.75
  pop2 <- wide[wide$Stratum == "Pop2", ]
  expect_equal(pop2$A, 0.25)
  expect_equal(pop2$B, 0.75)
})

test_that("returns a leaflet htmlwidget", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaflet.minicharts")
  df    <- make_test_data()
  freqs <- frequencies(df, loci = "TPI", stratum = "Population")
  map   <- make_map(df)

  result <- addAlleleFrequencies(map, freqs)
  expect_true(inherits(result, "leaflet"))
  expect_true(inherits(result, "htmlwidget"))
})

test_that("pipe pattern works end-to-end", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaflet.minicharts")
  df    <- make_test_data()
  freqs <- frequencies(df, loci = "TPI", stratum = "Population")

  result <- strata_coordinates(df, stratum = "Population") |>
    leaflet::leaflet() |>
    leaflet::addTiles() |>
    addAlleleFrequencies(freqs)

  expect_true(inherits(result, "leaflet"))
})
