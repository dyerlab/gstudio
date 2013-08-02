context("to_mv.R")



test_that("locus",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  loci <- c(AA, AB, BB)
  
  m <- to_mv( loci )
  expect_that( m, is_a("matrix"))
  expect_that( dim(m), is_equivalent_to(c(3,2)))
  expect_that( m[1,], is_equivalent_to( c(1,0) ))

})


test_that( "data.frame", {
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  loci <- c(AA, AB, BB)
  df <- data.frame( ID=1:3, POP=c("A","B","C"), Locus1=loci, Locus2=rev(loci) )
  
  # no loci error
  expect_that( to_mv( df[,1:2 ] ), throws_error() )
  

  m <- to_mv( df[,3:4] )
  expect_that( m, is_a("matrix"))
  expect_that( is.numeric(m), is_true() )
  expect_that( ncol(m), equals(4))
  expect_that( m[1,], is_equivalent_to( c(1,0,0,1)))
   
  m <- to_mv( df[,3:4], drop.allele=TRUE)
  expect_that( m, is_a("matrix"))
  expect_that( is.numeric(m), is_true())
  expect_that( ncol(m), equals(2) )
  expect_that( m[1,], is_equivalent_to( c(1,0)))
  
  
})
