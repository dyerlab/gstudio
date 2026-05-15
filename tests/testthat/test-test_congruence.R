test_that("returns htest for two overlapping graphs", {
  A <- matrix(0, nrow = 4, ncol = 4)
  B <- A

  A[1,2] <- A[1,3] <- A[2,3] <- A[3,4] <- 1
  B[1,2] <- B[1,3] <- B[1,4] <- B[2,4] <- B[2,3] <- 1

  A <- A + t(A)
  B <- B + t(B)
  rownames(A) <- colnames(A) <- rownames(B) <- colnames(B) <- LETTERS[1:4]

  graph1 <- as.popgraph(A)
  graph2 <- as.popgraph(B)

  ret <- test_congruence(graph1, graph2)
  expect_s3_class(ret, "htest")
  expect_equal(ret$parameter, c(df = 4))
})

test_that("errors when graphs share no nodes", {
  A <- matrix(0, nrow = 4, ncol = 4)
  graphA <- graph_from_adjacency_matrix(A, mode = "undirected")

  B <- matrix(0, nrow = 4, ncol = 4)
  B[1,2] <- B[1,3] <- B[2,3] <- B[3,4] <- 1
  B <- B + t(B)
  rownames(B) <- colnames(B) <- LETTERS[1:4]
  graph1 <- as.popgraph(B)

  expect_error(test_congruence(graph1, graphA))
})
