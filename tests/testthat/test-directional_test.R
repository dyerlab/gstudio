# Tests for directional_test(): the a-priori, axis-oriented nonparametric
# directional-bias test, including a check that the degree-expected center
# exactly cancels equal-distance arc weights.

make_path4 <- function(weights = c(1, 1, 1)) {
  A <- matrix(0, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  A["A", "B"] <- A["B", "A"] <- weights[1]
  A["B", "C"] <- A["C", "B"] <- weights[2]
  A["C", "D"] <- A["D", "C"] <- weights[3]
  g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected",
                                           weighted = TRUE, diag = FALSE)
  class(g) <- c("popgraph", class(g))
  g
}

test_that("degree center exactly cancels equal-distance arc weights", {
  g   <- make_path4(c(1, 1, 1))                 # all distances equal
  ori <- c(A = 1, B = 2, C = 3, D = 4)
  rd  <- directional_test(g, orientation = ori, center = "degree")
  expect_equal(nrow(rd$edges), 3L)
  expect_true(all(abs(rd$edges$deviation) < 1e-8))

  # Against a naive 0.5, the degree imbalance at the two leaf edges shows up
  # (and points in opposite axis directions), while the interior edge does not.
  rh <- directional_test(g, orientation = ori, center = "half")
  expect_gt(abs(rh$edges$deviation[rh$edges$from == "A"]), 0.1)
  expect_gt(abs(rh$edges$deviation[rh$edges$to   == "D"]), 0.1)
  expect_lt(abs(rh$edges$deviation[rh$edges$from == "B"]), 1e-8)
})

test_that("returns a valid, well-shaped result on unequal weights", {
  g   <- make_path4(c(2.1, 1.2, 2.8))
  res <- directional_test(g, orientation = c(A = 1, B = 2, C = 3, D = 4))
  expect_named(res$test, c("n_edges", "n_forward", "estimate", "statistic",
                           "p_value", "method", "alternative", "center"))
  expect_equal(res$test$n_edges, 3L)
  expect_true(res$test$p_value >= 0 && res$test$p_value <= 1)
  expect_true(all(c("from", "to", "w_forward", "center", "deviation") %in%
                    names(res$edges)))
})

test_that("orientation ties drop the affected edges", {
  g   <- make_path4(c(2.1, 1.2, 2.8))
  res <- directional_test(g, orientation = c(A = 1, B = 1, C = 2, D = 3))
  expect_lt(res$test$n_edges, 3L)        # A-B tie removed
})

test_that("sign test path runs and respects alternative", {
  g  <- make_path4(c(2.1, 1.2, 2.8))
  rs <- directional_test(g, orientation = c(A = 1, B = 2, C = 3, D = 4),
                         test = "sign", alternative = "two.sided")
  expect_identical(rs$test$method, "Sign test")
  expect_true(rs$test$p_value >= 0 && rs$test$p_value <= 1)
})

test_that("input validation", {
  g <- make_path4()
  gr <- igraph::make_ring(3)             # undirected, no weight attribute
  expect_error(directional_test(gr, orientation = 1:3), "weight")
  expect_error(directional_test(g, orientation = c(A = 1, B = 2)),
               "value for one or more")
})
