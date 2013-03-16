context("locus.R")

test_that("creating new ones", {

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
  locNULL <- locus()
  
  
  expect_that( AB, is_a("locus") )
  expect_that( AB, prints_text("A:B"))  
  expect_that( is.na(locNULL), is_true())
    
  loc.snp0 <- locus( 0, is.snp.minor=T )
  loc.snp1 <- locus( 1, is.snp.minor=T ) 
  loc.snp2 <- locus( 2, is.snp.minor=T )
  expect_that( loc.snp0, is_a("locus") )
  expect_that( loc.snp1, is_a("locus") )
  expect_that( loc.snp2, is_a("locus") )
  expect_that( loc.snp0, prints_text("A:A") )
  expect_that( loc.snp1, prints_text("A:B") )
  expect_that( loc.snp2, prints_text("B:B") )

  lst <- as.list( loci )
  expect_that( lst, is_a("list") )
  expect_that( length(lst), equals(10) )
  
  df <- data.frame( Pop=1:10, PGM=loci)
  expect_that( df, is_a("data.frame"))
  expect_that( names(df), is_equivalent_to( c("Pop","PGM") ) )
  expect_that( df[1,1], is_a("integer"))
  expect_that( df[1,2], is_a("locus"))
  expect_that( dim( df ), is_equivalent_to( c(10,2)))
}
)


test_that( "Vectors of Input", {
  
  snp <- c(0,1,2)
  loc <- NULL
  loc <- locus( snp, is.snp.minor=TRUE)
  expect_that( loc, is_a("locus"))
  expect_that( length(loc), equals(3) )
  
  separated <- c( "A:B", "A:C", "B:C" )
  loc <- NULL
  loc <- locus( separated, is.separated=TRUE )
  expect_that( loc, is_a("locus"))
  expect_that( length(loc), equals(3) )
  
  twocol <- matrix( c(1,2,1,1,2,2), ncol=2, byrow=T)
  loc <- NULL
  loc <- locus( twocol )
  expect_that( loc, is_a("locus"))
  expect_that( length(loc), equals(3) )
  
  zyme <- c( "AA","AB","BB")
  loc <- NULL
  loc <- locus( zyme , is.zyme=TRUE)
  expect_that( loc, is_a("locus"))
  expect_that( length(loc), equals(3) )
  
})



test_that( "Operations", {
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  
  expect_that( AA==AA, is_true() )
  expect_that( AA==BB, is_false() )
  expect_that( AA!=AC, is_true() )
  expect_that( AA!=AA, is_false())

  # addition operator
  off <- AA+BB
  expect_that( off, is_a("locus") )
  expect_that( ploidy(off), equals(2) )
  expect_that( is.heterozygote(off), is_true() )
  expect_that( off, prints_text("A:B") )
  
  off <- AA-AA
  expect_that( off, is_a("locus"))
  expect_that( off, prints_text("A") )
  
  off <- AB-AB
  expect_that( off, is_a("locus") )
  expect_that( off, prints_text("A:B") )
  
})













