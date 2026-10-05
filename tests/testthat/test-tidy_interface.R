# Tidy-pipeline behaviour shared across the package: grouped data frames use
# their grouping as the stratum, tibbles behave like data.frames, and every
# test returns an 'htest' that broom can tidy.

suppressPackageStartupMessages(library(dplyr))

same <- function(a, b) {
  if (igraph::is_igraph(a)) {
    a <- igraph::as_adjacency_matrix(a, attr = "weight", sparse = FALSE)
    b <- igraph::as_adjacency_matrix(b, attr = "weight", sparse = FALSE)
  }
  isTRUE(all.equal(as.data.frame(a), as.data.frame(b), check.attributes = FALSE))
}

test_that("group_by() supplies the stratum when none is given", {
  data(arapat, package = "gstudio", envir = environment())
  gr <- arapat |> group_by(Species)

  expect_true(same(genetic_diversity(gr, mode = "He"),
                   genetic_diversity(arapat, stratum = "Species", mode = "He")))
  expect_true(same(genetic_structure(gr), genetic_structure(arapat, stratum = "Species")))
  expect_true(same(genetic_distance(gr, mode = "Nei"),
                   genetic_distance(arapat, stratum = "Species", mode = "Nei")))
  expect_true(same(frequencies(gr), frequencies(arapat, stratum = "Species")))
  expect_true(same(frequency_matrix(gr), frequency_matrix(arapat, stratum = "Species")))
  expect_true(same(genotype_counts(gr), genotype_counts(arapat, stratum = "Species")))
  expect_true(same(allele_counts(gr, locus = "LTRS"),
                   allele_counts(arapat, locus = "LTRS", stratum = "Species")))
  expect_true(same(suppressWarnings(population_graph(gr)),
                   suppressWarnings(population_graph(arapat, stratum = "Species"))))
  expect_true(same(strata_coordinates(gr), strata_coordinates(arapat, stratum = "Species")))
})

test_that("an explicit stratum overrides the grouping", {
  data(arapat, package = "gstudio", envir = environment())
  gr <- arapat |> group_by(Species)
  expect_true(same(strata_coordinates(gr, stratum = "Population"),
                   strata_coordinates(arapat, stratum = "Population")))
  # "Population" is also the default value, so this checks the user's choice is
  # recognised as explicit rather than mistaken for the default
  expect_true(same(genetic_structure(gr, stratum = "Population"),
                   genetic_structure(arapat, stratum = "Population")))
  expect_true(same(genetic_distance(gr, stratum = "Population", mode = "Nei"),
                   genetic_distance(arapat, stratum = "Population", mode = "Nei")))
  expect_true(same(frequencies(gr, stratum = "Population"),
                   frequencies(arapat, stratum = "Population")))
  expect_true(same(frequency_matrix(gr, stratum = "Population"),
                   frequency_matrix(arapat, stratum = "Population")))
  expect_true(same(genetic_diversity(gr, stratum = "Population", mode = "He"),
                   genetic_diversity(arapat, stratum = "Population", mode = "He")))
})

test_that("tibbles work like data.frames", {
  data(arapat, package = "gstudio", envir = environment())
  tb <- as_tibble(arapat)
  expect_true(same(suppressWarnings(population_graph(tb, stratum = "Species")),
                   suppressWarnings(population_graph(arapat, stratum = "Species"))))
  expect_true(same(genetic_distance(tb, stratum = "Species", mode = "Nei"),
                   genetic_distance(arapat, stratum = "Species", mode = "Nei")))
  expect_true(same(strata_coordinates(tb), strata_coordinates(arapat)))
})

test_that("every test returns an htest that broom can tidy", {
  skip_if_not_installed("broom")
  data(gravity, package = "gstudio", envir = environment())
  g    <- population_graph(gravity)
  deme <- as.integer(sub("Pop", "", igraph::V(g)$name))
  set.seed(1)
  tests <- list(
    source_sink_test       = source_sink_test(g, x = deme, nperm = 49),
    ibgd_cgd               = ibgd(g, x = deme, nperm = 49),
    ibgd_pgd               = ibgd(g, x = deme, mode = "pgd"),
    directional_test       = directional_test(g, orientation = deme),
    asymmetry_existence    = asymmetry_significance(g, nperm = 19))
  for (nm in names(tests)) {
    expect_s3_class(tests[[nm]], "htest")
    td <- broom::tidy(tests[[nm]])
    expect_equal(nrow(td), 1L, label = nm)
    expect_true(all(c("statistic", "method") %in% names(td)), label = nm)
  }
  # tidied tests and per-edge tables share column names, so they stack
  edges <- asymmetry_significance(g, mode = "mechanism", nperm = 19)
  expect_true(all(c("statistic", "p.value") %in% names(edges)))
})
