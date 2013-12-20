context("minus_mom.R")

test_that( "testing",{
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
  
  loci <- c(AA,AB,AC,AD,BB,BC,BD,CD,CD,DD) 
  offID <- c(0 ,1,2 , 3, 0, 1, 2,1, 0, 2 )
  momID <- c(rep("A",4), rep("B",3), rep("C",3))
  
  df <- data.frame( ID=factor(momID), OffID=factor(offID), TPI=loci )
  
  expect_that( minus_mom("A"), throws_error() )
  expect_that( minus_mom(data.frame(x=3)), throws_error() )
  expect_that( minus_mom( df, "Bob"), throws_error() )
  expect_that( minus_mom( df,"ID","Bob"), throws_error() )
  
  mm <- minus_mom( df )
  expect_that( mm, is_a("data.frame") )
  expect_that( dim(mm), is_equivalent_to( c(7,3) ) )
  expect_that( names(mm), is_equivalent_to( c("ID","OffID","TPI")) )
  
  expect_that( as.character(mm$TPI), is_equivalent_to(c("B","C","D","C","D","C:D","D")))
  

  
})
