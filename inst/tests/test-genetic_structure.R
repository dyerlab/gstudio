context("genetic_structure.R")


test_that("checking",{
  loci <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  pops <- rep( c("A","B"), each=2 )
  df <- data.frame( Population=pops, TPI=loci )
  
#  gs <- genetic_structure( df, verbose=FALSE  )
#  expect_that( gs, is_a("genetic_structure") )
#  expect_that( gs$mode, equals( "Gst" ) ) 
#  expect_that( gs$loci, is_a("list") )
#  expect_that( gs$loci$TPI, is_a("structure_statistic") )
#  expect_that( gs$loci$TPI$estimate, equals(-0.2/3))
  
})