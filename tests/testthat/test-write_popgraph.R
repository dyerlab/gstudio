context("write_popgraph.R")

test_that("testing", {

  expect_that( write_popgraph(FALSE), throws_error() )

  a <- matrix( 0,nrow=4,ncol=4)
  a[1,2] <- a[1,3] <- a[2,3] <- a[1,4] <-1
  a <- a + t(a)
  graph <- as.popgraph(a)

  expect_that( write_popgraph(graph), throws_error() )
}
)

test_that("jgf format exports valid JSON with correct structure", {
  a <- matrix(0, nrow = 4, ncol = 4)
  a[1,2] <- a[1,3] <- a[2,3] <- a[1,4] <- 2
  a <- a + t(a)
  rownames(a) <- colnames(a) <- LETTERS[1:4]
  graph <- as.popgraph(a)

  tmp <- tempfile(fileext = ".json")
  write_popgraph(graph, tmp, format = "jgf", label = "Test Graph")

  doc <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_true(!is.null(doc$graph))
  expect_equal(doc$graph$label, "Test Graph")
  expect_true(!is.null(doc$graph$nodes))
  expect_true(!is.null(doc$graph$edges))
  expect_true("A" %in% names(doc$graph$nodes))
  expect_true(!is.null(doc$graph$nodes$A$metadata$size))
  expect_true(!is.null(doc$graph$nodes$A$metadata$color))

  # Each edge has source, target, and metadata with weight + distance
  edge1 <- doc$graph$edges[[1]]
  expect_true(!is.null(edge1$source))
  expect_true(!is.null(edge1$target))
  expect_true(!is.null(edge1$metadata$weight))

  unlink(tmp)
})

test_that("jgf format embeds loci metadata", {
  a <- matrix(0, nrow = 3, ncol = 3)
  a[1,2] <- a[2,3] <- 1
  a <- a + t(a)
  rownames(a) <- colnames(a) <- c("Pop1","Pop2","Pop3")
  graph <- as.popgraph(a)

  loci_df <- data.frame(
    name       = c("TPI", "MP20"),
    chromosome = c("1", "3"),
    position   = c(12345, 98765),
    stringsAsFactors = FALSE
  )

  tmp <- tempfile(fileext = ".json")
  write_popgraph(graph, tmp, format = "jgf", loci = loci_df)

  doc <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  loci_out <- doc$graph$metadata$loci
  expect_equal(length(loci_out), 2)
  expect_equal(loci_out[[1]]$name, "TPI")
  expect_equal(loci_out[[1]]$chromosome, "1")
  expect_equal(loci_out[[2]]$name, "MP20")
  expect_equal(loci_out[[2]]$position, 98765)

  unlink(tmp)
})

test_that("to_jgf returns JSON string when no file given", {
  a <- matrix(0, nrow = 2, ncol = 2)
  a[1,2] <- 1; a[2,1] <- 1
  rownames(a) <- colnames(a) <- c("X","Y")
  graph <- as.popgraph(a)

  result <- to_jgf(graph)
  expect_true(is.character(result))
  doc <- jsonlite::fromJSON(result, simplifyVector = FALSE)
  expect_true(!is.null(doc$graph$nodes$X))
  expect_true(!is.null(doc$graph$nodes$Y))
})
