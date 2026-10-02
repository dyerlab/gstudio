
test_that("checking",{
  loci <- c( locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(1,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ),
             locus( c(2,3) ))

  expect_that( A("Bob"), throws_error() )
  expect_that( A(loci), is_equivalent_to(3) )
  expect_that( A(loci, min_freq=0.05), is_equivalent_to(2) )

  # Data.frame test
  df <- data.frame(TPI = loci, PGM = loci)
  res_df <- A(df)
  expect_s3_class(res_df, "data.frame")
  expect_equal(res_df$A, c(3, 3))

  # Data.frame with min_freq > 0
  res_min <- A(df, min_freq = 0.05)
  expect_true("A95" %in% names(res_min))
  expect_equal(res_min$A95, c(2, 2))

  # Specific loci subset
  expect_equal(nrow(A(df, loci = "TPI")), 1)
}
)