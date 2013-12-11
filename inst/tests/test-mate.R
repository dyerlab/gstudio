context("mate.R")

test_that( "testing",{
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  
  loci <- c(AA,AB) 
  momID <- c("A","B") 
  df <- data.frame( ID=factor(momID), TPI=loci )
  
  expect_that( mate(), throws_error())
  expect_that( mate(df[1,]), throws_error())
  
  o <- mate(df[1,], df[1,], N=1)
  expect_that( o , is_a("data.frame") )
  expect_that( names(o), is_equivalent_to( c("ID","TPI")))
  expect_that( nrow(o), equals(1) )
  
  df$OffID=0
  o <- mate(df[1,], df[1,], N=10)
  expect_that( nrow(o), equals(10) )
  expect_that( names(o), is_equivalent_to( c("ID","OffID","TPI")))
  expect_that( as.character(o$ID[1]), equals("A"))
  expect_that( as.character(o$OffID[1]), equals("1"))
  expect_that( o$TPI[1], prints_text( "A:A") )
  
  
  
})
