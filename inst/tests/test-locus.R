context("locus.R Diploid")

test_that("creating new ones", {

  AB <- locus( c("A","B") )
  loci <- c(AB,AB,AB)
  locNULL <- locus()
  expect_that( AB, is_a("locus") )
  expect_that( AB, prints_text("A:B"))  
  expect_that( nchar(locNULL), equals(0) )

  expect_that(loci, is_a("locus"))
  expect_that(length(loci), equals(3))
  expect_that( loci[1], equals(AB))
})



test_that("passing NA", {
  loc1 <- locus( NA )
  expect_that( is.na(loc1), is_true())
  expect_that( loc1, prints_text(""))
  
  loc2 <- locus( "NA:NA", type="separated")
  expect_that( is.na(loc2), is_true())
  expect_that( loc2, prints_text(""))
})



test_that("List context",{
  AB <- locus( c("A","B") )
  loci <- c(AB,AB,AB)
  
  lst <- as.list( loci )
  expect_that( lst, is_a("list") )
  expect_that( length(lst), equals(3) )
})



context("locus.R SNPs")
test_that("SNP Creation", {
  loc.snp0 <- locus( 0, type="snp" )
  loc.snp1 <- locus( 1, type="snp" ) 
  loc.snp2 <- locus( 2, type="snp" )
  loc.snp3 <- locus( 3, type="snp" )
  
  expect_that( loc.snp0, is_a("locus") )
  expect_that( loc.snp1, is_a("locus") )
  expect_that( loc.snp2, is_a("locus") )
  expect_that( loc.snp3, is_a("locus") )
  expect_that( loc.snp0, prints_text("A:A") )
  expect_that( loc.snp1, prints_text("A:B") )
  expect_that( loc.snp2, prints_text("B:B") )
  expect_that( loc.snp3, prints_text("") )
 
  loci <- locus( c(0,1,2), type="snp")
  expect_that( loci, is_a("locus") )
  expect_that( loci[1], equals( locus(c("A","A"))))
  expect_that( loci[2], equals( locus(c("A","B"))))
  expect_that( loci[3], equals( locus(c("B","B"))))
}
)


context("Locus.R columns ")
test_that( "Separated", {
  x <- c( "A:B", "A:C", "B:C" )
  loc <- locus( x, type="separated" )
  expect_that( loc, is_a("locus"))
  expect_that( length(loc), equals(3) )
  expect_that( loc[1], equals( locus( c("A","B"))))  
})




context("Locus.R separated ")
test_that( "Columns", {
  twocol <- matrix( c(1,2,1,1,2,2), ncol=2, byrow=T)
  loc <- locus( twocol, type="column" )
  expect_that( loc, is_a("locus"))
  expect_that( length(loc), equals(3) )
})




context( "Locus.R zyme-play")
test_that("testing",{
  zyme <- c( "AA","AB","BB")
  loc <- NULL
  loc <- locus( zyme, type="zyme")
  expect_that( loc, is_a("locus"))
  expect_that( length(loc), equals(3) )  
})



context("Locus.R operators")
test_that( "Operations", {
  AA <- locus( c("A","A") )
  AB <- locus( c("A","B") )
  BB <- locus( c("B","B") )
  AC <- locus( c("A","C") )
  BC <- locus( c("B","C") )
  
  
  expect_that( AA==AA, is_true() )
  expect_that( AA==BB, is_false() )
  expect_that( AA!=AC, is_true() )
  expect_that( AA!=AA, is_false())

  # addition operator
  off <- AA+BB
  expect_that( off, is_a("locus") )
  expect_that( ploidy(off), equals(2) )
  expect_that( is_heterozygote(off), is_true() )
  expect_that( as.character(off)=="A:B", is_true() ) 
  
  off <- AA-AA
  expect_that( off, is_a("locus"))
  expect_that( as.character(off)=="A", is_true() ) 
  
  off <- AB-AB
  expect_that( off, is_a("locus") )
  expect_that( as.character(off)=="A:B", is_true() )
  
  off <- BB-AB
  expect_that( as.character(off)=="B", is_true())
    
})
