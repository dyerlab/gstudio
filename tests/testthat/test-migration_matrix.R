context("migration_matrix.R")

test_that("island model has correct structure and rows sum to 1", {
  mat <- migration_matrix(4, model = "island", m = 0.12)
  expect_equal(nrow(mat), 4)
  expect_equal(ncol(mat), 4)
  # Diagonal
  expect_equal(unname(diag(mat)), rep(1 - 0.12, 4))
  # Off-diagonal
  off_diag <- mat[1, 2]
  expect_equal(off_diag, 0.12 / 3)
  # Rows sum to 1
  expect_equal(unname(rowSums(mat)), rep(1, 4), tolerance = 1e-10)
})

test_that("1D stepping stone: edge pops have 1 neighbor, interior have 2", {
  mat <- migration_matrix(4, model = "stepping_stone_1d", m = 0.1)
  expect_equal(unname(rowSums(mat)), rep(1, 4), tolerance = 1e-10)
  # Pop 1 (edge): only neighbor is pop 2
  expect_equal(mat[1, 2], 0.1)
  expect_equal(mat[1, 3], 0)
  expect_equal(mat[1, 4], 0)
  # Pop 2 (interior): neighbors are pop 1 and 3
  expect_equal(mat[2, 1], 0.05)
  expect_equal(mat[2, 3], 0.05)
  expect_equal(mat[2, 4], 0)
})

test_that("2D stepping stone: corner=2, edge=3, interior=4 neighbors", {
  # 3x3 grid = 9 pops
  mat <- migration_matrix(9, model = "stepping_stone_2d", m = 0.2,
                          nr = 3, nc = 3)
  expect_equal(nrow(mat), 9)
  expect_equal(unname(rowSums(mat)), rep(1, 9), tolerance = 1e-10)

  # Pop 1 is corner (row=1, col=1): 2 neighbors -> m/2 each
  n_neighbors_1 <- sum(mat[1, ] > 0 & seq_len(9) != 1)
  expect_equal(n_neighbors_1, 2)

  # Pop 2 is edge (row=1, col=2): 3 neighbors -> m/3 each
  n_neighbors_2 <- sum(mat[2, ] > 0 & seq_len(9) != 2)
  expect_equal(n_neighbors_2, 3)

  # Pop 5 is interior (row=2, col=2): 4 neighbors -> m/4 each
  n_neighbors_5 <- sum(mat[5, ] > 0 & seq_len(9) != 5)
  expect_equal(n_neighbors_5, 4)
})

test_that("distance model: nearer pops get higher migration", {
  coords <- matrix(c(0, 0, 1, 0, 10, 0), ncol = 2, byrow = TRUE)
  mat <- migration_matrix(3, model = "distance", m = 0.1, coords = coords)
  expect_equal(unname(rowSums(mat)), rep(1, 3), tolerance = 1e-10)
  # From pop 1: pop 2 (dist=1) should get more than pop 3 (dist=10)
  expect_true(mat[1, 2] > mat[1, 3])
})

test_that("migration_event validates start/end ordering", {
  mat <- migration_matrix(3, model = "island", m = 0.05)
  expect_error(migration_event(mat, start = 5, end = 3))
  expect_error(migration_event(mat, start = 0))
  evt <- migration_event(mat, start = 1, end = 10)
  expect_s3_class(evt, "migration_event")
})

test_that("named populations carry through", {
  mat <- migration_matrix(c("Alpha", "Beta", "Gamma"), model = "island", m = 0.1)
  expect_equal(rownames(mat), c("Alpha", "Beta", "Gamma"))
  expect_equal(colnames(mat), c("Alpha", "Beta", "Gamma"))
})
