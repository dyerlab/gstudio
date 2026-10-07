
test_that("testing", {

  A <- matrix(0,nrow=4,ncol=4)
  B <- A
  
  A[1,2] <- A[1,3] <- A[2,3] <- A[3,4] <- 1
  B[1,2] <- B[1,3] <- B[1,4] <- B[2,4] <- B[2,3] <- 1
  
  A <- A + t(A)
  B <- B + t(B)
  
  rownames(A) <- colnames(A) <- rownames(B) <- colnames(B) <- LETTERS[1:4]
  
  graph1 <- as.popgraph(A)
  graph2 <- as.popgraph(B)
  
  cong <- congruence_topology(graph1,graph2)
  expect_that( cong, is_a("igraph") )
  expect_that( cong, is_a("popgraph"))
  expect_that( length(V(cong)), equals(4) )
  expect_that( length(E(cong)), equals(3) )
}
)

test_that("congruence_topology carries the parents' vertex attributes", {
  A <- matrix(0, 5, 5, dimnames = list(LETTERS[1:5], LETTERS[1:5]))
  B <- A[1:4, 1:4]
  A[1, 2] <- A[1, 3] <- A[2, 3] <- A[3, 4] <- A[4, 5] <- 1
  B[1, 2] <- B[1, 3] <- B[1, 4] <- B[2, 4] <- B[2, 3] <- 1
  g1 <- as.popgraph(A + t(A))
  g2 <- as.popgraph(B + t(B))
  igraph::V(g1)$Latitude  <- c(10, 20, 30, 40, 50)
  igraph::V(g2)$Latitude  <- c(10, 20, 30, 40)            # same on shared nodes
  igraph::V(g1)$Region    <- c("N", "N", "S", "S", "S")    # only in graph1
  igraph::V(g2)$Elevation <- c(100, 200, 300, 400)         # only in graph2
  igraph::V(g1)$size      <- 1:5                           # differs between graphs
  igraph::V(g2)$size      <- 4:1

  cong <- suppressWarnings(congruence_topology(g1, g2))
  expect_equal(igraph::V(cong)$name, LETTERS[1:4])
  expect_equal(igraph::V(cong)$Latitude, c(10, 20, 30, 40))
  expect_equal(igraph::V(cong)$Region, c("N", "N", "S", "S"))
  expect_equal(igraph::V(cong)$Elevation, c(100, 200, 300, 400))
  expect_null(igraph::V(cong)$size)
  expect_equal(igraph::V(cong)$size.1, 1:4)
  expect_equal(igraph::V(cong)$size.2, 4:1)
  expect_s3_class(plot(cong), "ggplot")

  # undecorated parents give an undecorated topology
  expect_equal(igraph::vertex_attr_names(congruence_topology(as.popgraph(B + t(B)), as.popgraph(B + t(B)))),
               "name")
})
