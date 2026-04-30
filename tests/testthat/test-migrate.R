context("migrate.R")

# Helper: build a multi-population data frame
.make_migpop <- function(pops = c("A", "B"), n = 10) {
  freqs <- data.frame(
    Population = rep(pops, each = 2),
    Locus = "L1",
    Allele = rep(c("01", "02"), length(pops)),
    Frequency = 0.5
  )
  freqs$Population <- factor(freqs$Population, ordered = TRUE)
  make_populations(freqs, N = n)
}


test_that("total individuals are conserved and no NA rows produced", {
  set.seed(1)
  pop <- .make_migpop(c("A", "B"), n = 10)
  result <- migrate(pop, m = 0.1)
  expect_equal(nrow(result), 20)
  expect_false(any(is.na(result$L1)), label = "no NA loci")
  expect_equal(sort(unique(result$Population)), c("A", "B"))
})

test_that("4-pop island model does not crash and conserves total N", {
  # This test specifically guards against the rounding bug (sum > n_i) that
  # previously caused sample() to error for a 4-pop island with m=0.2
  set.seed(2)
  pop <- .make_migpop(c("A", "B", "C", "D"), n = 10)
  mat <- migration_matrix(c("A", "B", "C", "D"), model = "island", m = 0.2)
  result <- migrate(pop, m = mat)
  expect_equal(nrow(result), 40)
  expect_false(any(is.na(result$L1)), label = "no NA loci after 4-pop island")
})

test_that("1D stepping stone conserves total N and produces no NAs", {
  # Previously, interior pops silently lost 1 individual/gen due to rounding sum < n_i
  set.seed(3)
  pop <- .make_migpop(paste0("P", 1:5), n = 20)
  mat <- migration_matrix(paste0("P", 1:5), model = "stepping_stone_1d", m = 0.1)
  result <- migrate(pop, m = mat)
  expect_equal(nrow(result), 100)
  expect_false(any(is.na(result$L1)), label = "no NA loci after 1D SS")
})

test_that("2D stepping stone (3x3) does not crash and conserves total N", {
  # Previously, edge pops (3 neighbors) caused a crash because rounding sum > n_i
  set.seed(4)
  pnames <- paste0("Pop", 1:9)
  pop <- .make_migpop(pnames, n = 10)
  mat <- migration_matrix(9, model = "stepping_stone_2d", m = 0.2, nr = 3, nc = 3)
  colnames(mat) <- rownames(mat) <- pnames
  result <- migrate(pop, m = mat)
  expect_equal(nrow(result), 90)
  expect_false(any(is.na(result$L1)), label = "no NA loci after 2D SS")
})

test_that("named matrix with reversed order aligns correctly (no crash, conserves N)", {
  set.seed(5)
  pop <- .make_migpop(c("A", "B"), n = 20)
  mat <- migration_matrix(c("B", "A"), model = "island", m = 0.1)
  result <- migrate(pop, m = mat)
  expect_equal(nrow(result), 40)
  expect_false(any(is.na(result$L1)))
})

test_that("m = 0: every individual stays in their original population", {
  set.seed(6)
  pop <- .make_migpop(c("A", "B"), n = 15)
  result <- migrate(pop, m = 0)
  expect_equal(sum(result$Population == "A"), 15)
  expect_equal(sum(result$Population == "B"), 15)
})

test_that("simulate_pop with 4-pop island maintains constant population sizes", {
  # Verifies that the migrate() fix + simulate_pop's resampling maintains N per pop
  set.seed(7)
  pop <- .make_migpop(c("A", "B", "C", "D"), n = 20)
  mat  <- migration_matrix(c("A", "B", "C", "D"), model = "island", m = 0.2)
  evt  <- migration_event(mat, start = 1)
  result <- simulate_pop(pop, ngen = 5, migration = evt, verbose = FALSE)
  expect_equal(nrow(result), 80)
  for (p in c("A", "B", "C", "D"))
    expect_equal(sum(result$Population == p), 20, label = paste("pop", p, "size"))
})

test_that("simulate_pop with 1D stepping stone maintains constant population sizes", {
  set.seed(8)
  pop <- .make_migpop(paste0("P", 1:5), n = 20)
  mat  <- migration_matrix(paste0("P", 1:5), model = "stepping_stone_1d", m = 0.1)
  evt  <- migration_event(mat, start = 1)
  result <- simulate_pop(pop, ngen = 5, migration = evt, verbose = FALSE)
  expect_equal(nrow(result), 100)
  for (p in paste0("P", 1:5))
    expect_equal(sum(result$Population == p), 20, label = paste("pop", p, "size"))
})

test_that("simulate_pop with 2D stepping stone maintains constant population sizes", {
  set.seed(9)
  pnames <- paste0("Pop", 1:9)
  pop <- .make_migpop(pnames, n = 20)
  mat  <- migration_matrix(9, model = "stepping_stone_2d", m = 0.2, nr = 3, nc = 3)
  colnames(mat) <- rownames(mat) <- pnames
  evt  <- migration_event(mat, start = 1)
  result <- simulate_pop(pop, ngen = 5, migration = evt, verbose = FALSE)
  expect_equal(nrow(result), 180)
  for (p in pnames)
    expect_equal(sum(result$Population == p), 20, label = paste("pop", p, "size"))
})
