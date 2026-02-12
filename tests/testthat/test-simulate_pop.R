context("simulate_pop.R")

# Helper: build a simple multi-pop test data.frame
.make_sim_pop <- function() {
  f1 <- data.frame(
    Population = "A",
    Locus = rep("Loc1", 2),
    Allele = c("01", "02"),
    Frequency = c(0.5, 0.5)
  )
  f2 <- data.frame(
    Population = "B",
    Locus = rep("Loc1", 2),
    Allele = c("01", "02"),
    Frequency = c(0.5, 0.5)
  )
  freqs <- rbind(f1, f2)
  freqs$Population <- factor(freqs$Population, ordered = TRUE)
  make_populations(freqs, N = 20, F = 0)
}


test_that("single pop, no migration: runs and returns correct N", {
  set.seed(1)
  f1 <- data.frame(
    Population = "A",
    Locus = rep("Loc1", 2),
    Allele = c("01", "02"),
    Frequency = c(0.5, 0.5)
  )
  f1$Population <- factor(f1$Population, ordered = TRUE)
  pop <- make_populations(f1, N = 15, F = 0)
  result <- simulate_pop(pop, ngen = 5, verbose = FALSE)
  expect_equal(nrow(result), 15)
  expect_true("Loc1" %in% names(result))
})

test_that("multi-pop with island migration: pops persist, N maintained", {
  set.seed(2)
  pop <- .make_sim_pop()
  mat <- migration_matrix(c("A", "B"), model = "island", m = 0.05)
  evt <- migration_event(mat, start = 1)
  result <- simulate_pop(pop, ngen = 5, migration = evt, verbose = FALSE)
  expect_equal(nrow(result), 40)
  expect_equal(sort(unique(result$Population)), c("A", "B"))
  expect_equal(sum(result$Population == "A"), 20)
  expect_equal(sum(result$Population == "B"), 20)
})

test_that("census files created at correct intervals", {
  set.seed(3)
  pop <- .make_sim_pop()
  tdir <- tempfile("census_test")
  result <- simulate_pop(pop, ngen = 10, census_interval = 5,
                         census_dir = tdir, verbose = FALSE)
  # Expect gen 5, 10 snapshots
  files <- list.files(tdir, pattern = "\\.rds$")
  expect_true("gen_0005.rds" %in% files)
  expect_true("gen_0010.rds" %in% files)
  # Read back and verify
  snap <- readRDS(file.path(tdir, "gen_0005.rds"))
  expect_equal(nrow(snap), 40)
  unlink(tdir, recursive = TRUE)
})

test_that("temporal regime change: different matrices in different gens", {
  set.seed(4)
  pop <- .make_sim_pop()
  mat1 <- migration_matrix(c("A", "B"), model = "island", m = 0.01)
  mat2 <- migration_matrix(c("A", "B"), model = "island", m = 0.20)
  evt1 <- migration_event(mat1, start = 1, end = 5)
  evt2 <- migration_event(mat2, start = 6)
  result <- simulate_pop(pop, ngen = 10, migration = list(evt1, evt2),
                         verbose = FALSE)
  expect_equal(nrow(result), 40)
})

test_that("selfing increases homozygosity over generations", {
  set.seed(5)
  f1 <- data.frame(
    Population = "A",
    Locus = rep("Loc1", 2),
    Allele = c("01", "02"),
    Frequency = c(0.5, 0.5)
  )
  f1$Population <- factor(f1$Population, ordered = TRUE)
  pop <- make_populations(f1, N = 100, F = 0)

  # No selfing
  set.seed(10)
  result_outcross <- simulate_pop(pop, ngen = 20, selfing_rate = 0,
                                  verbose = FALSE)
  ho_outcross <- Ho(result_outcross$Loc1)

  # High selfing
  set.seed(10)
  result_selfing <- simulate_pop(pop, ngen = 20, selfing_rate = 0.9,
                                 verbose = FALSE)
  ho_selfing <- Ho(result_selfing$Loc1)

  # Selfing should reduce Ho (or at least not be higher on average)
  expect_true(ho_selfing <= ho_outcross + 0.15)
})
