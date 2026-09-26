make_graph <- function(A) {
  A <- A + t(A)
  rownames(A) <- colnames(A) <- LETTERS[1:nrow(A)]
  as.popgraph(A)
}

test_that("animate_popgraphs writes a gif", {
  skip_if_not_installed("gifski")

  A <- matrix(0, 4, 4); A[1, 2] <- A[2, 3] <- A[3, 4] <- 1
  B <- matrix(0, 4, 4); B[1, 3] <- B[1, 4] <- B[2, 4] <- 1
  graphs <- list(First = make_graph(A), Second = make_graph(B))

  out <- tempfile(fileext = ".gif")
  set.seed(1)
  res <- animate_popgraphs(graphs, file = out, delay = 0.5, width = 200, height = 200)
  expect_equal(res, out)
  expect_true(file.exists(out))
  expect_identical(readBin(out, "raw", 3), charToRaw("GIF"))
})

test_that("animate_popgraphs accepts user layouts", {
  skip_if_not_installed("gifski")

  A <- matrix(0, 4, 4); A[1, 2] <- A[2, 3] <- A[3, 4] <- 1
  B <- matrix(0, 3, 3); B[1, 2] <- B[2, 3] <- 1
  graphs <- list(make_graph(A), make_graph(B))   # second graph lacks node D

  xy <- matrix(c(0, 1, 1, 0, 0, 0, 1, 1), ncol = 2, dimnames = list(LETTERS[1:4], NULL))
  out <- tempfile(fileext = ".gif")
  expect_silent(animate_popgraphs(graphs, file = out, layout = xy, width = 200, height = 200))
  expect_true(file.exists(out))

  coords <- data.frame(Stratum = LETTERS[1:4], Longitude = xy[, 1], Latitude = xy[, 2])
  expect_silent(animate_popgraphs(graphs, file = out, layout = coords, width = 200, height = 200))

  expect_silent(animate_popgraphs(graphs, file = out, layout = igraph::layout_in_circle,
                                  width = 200, height = 200))
})

test_that("animate_popgraphs validates input", {
  skip_if_not_installed("gifski")

  A <- matrix(0, 4, 4); A[1, 2] <- A[2, 3] <- 1
  g <- make_graph(A)
  out <- tempfile(fileext = ".gif")

  expect_error(animate_popgraphs(g, file = out), "list of graphs")
  expect_error(animate_popgraphs(list(), file = out), "non-empty")
  expect_error(animate_popgraphs(list(g, 1), file = out), "popgraph or igraph")
  expect_error(animate_popgraphs(list(g), file = out, delay = -1), "positive")
  expect_error(animate_popgraphs(list(g), file = out, layout = "bogus"), "Unknown layout")
  xy <- matrix(0, 2, 2, dimnames = list(c("A", "B"), NULL))
  expect_error(animate_popgraphs(list(g), file = out, layout = xy), "no coordinates for: C, D")
})
