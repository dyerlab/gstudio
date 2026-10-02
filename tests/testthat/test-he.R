
test_that("expected heterozygosity",{
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
  
  
  h <- He( loci )
  expect_that( h, is_equivalent_to(0.75) )

  # small.N correction on single locus
  h_small <- He( loci, small.N = TRUE )
  expect_true( h_small > h )

  # Data.frame test
  df <- data.frame(TPI = loci, PGM = loci)
  res_df <- He(df)
  expect_s3_class(res_df, "data.frame")
  expect_equal(res_df$He, c(0.75, 0.75))

  # Data.frame with small.N
  res_df_small <- He(df, small.N = TRUE)
  expect_true(all(res_df_small$He > res_df$He))
})