context("column_locus_type.R")

test_that("test",{
  
  df <- data.frame( Int=1:10 )
  df$CODOM1 <- rep( locus(1:1), 10 )
  df$CODOM2 <- rep( locus(2:3), 10 )
  df$AFLP1 <- locus( (rpois(10,lambda=20) %% 2), type="aflp") 
  
  expect_that( column_locus_type(), throws_error() )
  
  expect_that( column_locus_type(df), is_equivalent_to( c("CODOM1", "CODOM2", "AFLP")))
  expect_that( column_locus_type(df,locus_type = "codom"), is_equivalent_to( c("CODOM1", "CODOM2")))
  expect_that( column_locus_type(df,locus_type = "aflp"), is_equivalent_to( c("AFLP1")))
  
  x <- column_locus_type(df,locus_type = "snp")
  expect_that( length(x), equals(0))
  
})