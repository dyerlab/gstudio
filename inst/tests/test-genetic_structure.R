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
  
  gs <- genetic_structure( df  )
  expect_that( gs, is_a("data.frame") )
  expect_that( dim(gs), is_equivalent_to(c(1,5) ) )
  expect_that( names(gs), is_equivalent_to(c("Locus","Gst","Hs","Ht","P") ) )
  
  df$PGM <- rev(loci)
  
  gs <- genetic_structure( df, pairwise=TRUE )
  expect_that( gs, is_a("list") )
  expect_that( length(gs), equals(2) )
  expect_that( gs[[1]], is_a("matrix") )  
  
})