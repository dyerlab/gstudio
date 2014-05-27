context("frequencies.R")

test_that("frequencies.locus", {
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
  loc1 <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD)
  
  f <- frequencies( loc1 )
  expect_that( f, is_a("data.frame"))
  expect_that( names(f), is_equivalent_to(c("Allele","Frequency") ) )
  expect_that( unique(sort(alleles(loc1))), is_equivalent_to(sort(f$Allele) ) )
  expect_that( sum( f$Frequency ), equals(1))
    
})


test_that("frequencies.data.frame", {
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
  loc1 <- c(AA,AB,AC,AD,BB,BC,BD,CC,CD,DD)
  loc2 <- c(AA,AA,AC,AA,CC,CC,AC,CC,AA,AC)
  
  df <- data.frame( Population=c(rep("A",5),rep("B",5)), TPI=loc1, PGM=loc2 )
  f <- frequencies(df,loci="TPI")
  expect_that(f, is_a("data.frame"))
  expect_that(names(f), is_equivalent_to(c("Locus","Allele","Frequency")))
  
  f <- frequencies(df)
  expect_that( length(unique(f$Locus)), equals(2) )
  expect_that( sum( f$Frequency), equals(2) )
  
  expect_that( f <- frequencies( df, loci="bob"), throws_error() )
  expect_that( f <- frequencies( df, loci=c("bob","TPI")), throws_error() )
  expect_that( f <- frequencies( df, stratum="bob"), throws_error() )
  
  
  
})


test_that("frequencies.data.frame", {
  x <- matrix( abs( rnorm(30)), ncol=3)
  x <- x / rowSums(x)
  df <- data.frame( Loc1_1=x[,1], Loc1_2=x[,2], Loc1_3=x[,3])
  
  f <- frequencies(df)
  expect_that( f, is_a("data.frame"))
  expect_that( names(f), is_equivalent_to(c("Locus","Allele","Frequency")))
  expect_that( f$Allele, is_equivalent_to(c("A","B")))
  expect_that( f$Locus, is_equivalent_to(c("Loc1","Loc1")))
  expect_that( sum(f$Frequency), is_equivalent_to(1))

})





