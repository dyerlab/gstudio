context("genetic_structure.R")


test_that("checking",{
  loc1 <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,1) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,1) ),
             locus( c(2,2) ) )
  loc2 <- c( locus( c(1,1) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(1,2) ),
             locus( c(2,2) ),
             locus( c(2,1) ),
             locus( c(2,3) ),
             locus( c(2,3) ) )
  pops <- rep( c("A","B"), each=2 )
  df <- data.frame( Population=pops, TPI=loc1 )
  
  gs <- genetic_structure( df  )
  expect_that( gs, is_a("data.frame") )
  expect_that( dim(gs), is_equivalent_to(c(1,5) ) )
  expect_that( names(gs), is_equivalent_to(c("Locus","Gst","Hs","Ht","P") ) )
  
  df$PGM <- loc2
  gs <- genetic_structure( df  )
  expect_that( gs, is_a("data.frame") )
  expect_that( dim(gs), is_equivalent_to(c(3,5) ) )
  expect_that( names(gs), is_equivalent_to(c("Locus","Gst","Hs","Ht","P") ) )
  expect_that( gs$Locus, is_equivalent_to( c("TPI","PGM","Multilocus")))
  expect_that( gs$Gst[1] < gs$Gst[2], is_true())
  
  gs <- genetic_structure( df, pairwise=TRUE )
  expect_that( gs, is_a("matrix") )
  expect_that( dim(gs), is_equivalent_to( c(2,2)) ) 
  expect_that( all( is.na(diag(gs))), is_true() ) 
  expect_that( gs[1,2] == gs[2,1], is_true() )
  
  
  
  
})