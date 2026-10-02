test_that("genetic_distance error checks", {
  expect_error( genetic_distance( data.frame(X=1), mode="Bob") )
  expect_error( genetic_distance( numeric(10), mode="AMOVA") )
  expect_error( genetic_distance( data.frame(A=numeric(0)), mode="AMOVA") )
})

test_that("genetic_distance dispatches across all distance modes", {
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  AD <- locus( c("A","D") )
  BC <- locus( c("B","C") )
  BD <- locus( c("B","D") )
  CC <- locus( c("C","C") )
  CD <- locus( c("C","D") )
  DD <- locus( c("D","D") )
  loci <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD) 
  stratum <- c(rep("A",3), rep("B",3), rep("C",4) )
  df <- data.frame( Population=stratum, Locus=loci)
  
  # Individual modes
  d_amova <- genetic_distance(df, mode = "amova")
  expect_true(is.matrix(d_amova))
  expect_equal(dim(d_amova), c(10, 10))

  d_bray <- genetic_distance(df, mode = "bray")
  expect_true(is.matrix(d_bray))
  expect_equal(dim(d_bray), c(10, 10))

  # Stratum modes
  d_dps <- genetic_distance(df, mode = "dps")
  expect_equal(dim(d_dps), c(3, 3))

  d_cavalli <- genetic_distance(df, mode = "cavalli")
  expect_equal(dim(d_cavalli), c(3, 3))

  d_euclidean <- genetic_distance(df, mode = "euclidean")
  expect_equal(dim(d_euclidean), c(3, 3))

  d_jaccard <- genetic_distance(df, mode = "jaccard")
  expect_equal(dim(d_jaccard), c(3, 3))

  d_nei <- genetic_distance(df, mode = "nei")
  expect_equal(dim(d_nei), c(3, 3))

  d_ss <- genetic_distance(df, mode = "ss")
  expect_equal(dim(d_ss), c(3, 3))

  suppressWarnings(
    d_cgd <- genetic_distance(df, mode = "cgd")
  )
  expect_equal(dim(d_cgd), c(3, 3))
})
