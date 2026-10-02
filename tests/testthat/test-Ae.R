
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
  expect_that( Ae("Bob"), throws_error() )
  expect_that( Ae(loci), is_equivalent_to( 1/(1-He(loci))) )

  # Data.frame test
  df <- data.frame(TPI = loci, PGM = loci)
  res_df <- Ae(df)
  expect_s3_class(res_df, "data.frame")
  expect_equal(unname(res_df$Ae), unname(c(Ae(loci), Ae(loci))))

  # Specific loci subset
  expect_equal(nrow(Ae(df, loci = "TPI")), 1)
}
)