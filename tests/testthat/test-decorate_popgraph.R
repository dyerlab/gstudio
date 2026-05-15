
test_that("testing", {
  
  A <- matrix(0, nrow=4, ncol=4)
  A[1,2] <- A[2,3] <- A[1,3] <- A[3,4] <- 1
  A <- A + t(A)
  rownames(A) <- colnames(A) <- LETTERS[1:4]
  g <- as.popgraph( A )

  df <- data.frame(name=LETTERS[1:4], Position=c(1,2,3,4))  
  
  expect_that(decorate_graph( g, df ), throws_error() )
  
  g.df <- decorate_graph( g, df, stratum="name" )
      
  expect_that( g.df, is_a("igraph") )
  expect_that( g.df, is_a("popgraph"))

  expect_that( V(g.df)$Bob, is_a("NULL") )
  
  expect_that( V(g.df)$Position, is_a("numeric"))
  expect_that( V(g.df)$Position[1], equals(1) )
})


test_that("factor columns are stored as character labels, not integer codes", {
  A <- matrix(0, nrow = 3, ncol = 3)
  A[1,2] <- A[2,3] <- 1
  A <- A + t(A)
  rownames(A) <- colnames(A) <- c("X", "Y", "Z")
  g <- as.popgraph(A)

  df <- data.frame(
    name   = c("X", "Y", "Z"),
    Region = factor(c("North", "South", "North"), levels = c("North", "South"))
  )
  g2 <- decorate_graph(g, df, stratum = "name")

  expect_equal(V(g2)$Region, c("North", "South", "North"))
  expect_false(is.numeric(V(g2)$Region))
})
