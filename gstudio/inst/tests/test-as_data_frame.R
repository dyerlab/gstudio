context("as_data_frame.R")






test_that( "genetic_structure",{
  loci <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  pops <- rep( c("A","B"), each=4 )
  df <- data.frame( Population=pops, TPI=loci )
})



test_that( "locus", {
  loci <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  df <- data.frame( loci )
  
  expect_that( df, is_a("data.frame") )
  expect_that( names(df), is_equivalent_to("loci") )
  expect_that( df[1,1], is_a("locus") )
  expect_that( length(df[,1]), equals(8) )
  
})