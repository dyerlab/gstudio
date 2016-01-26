context("make_loci.R")

test_that( "testing make_loci",{

  f <- data.frame( Allele=c(0,1,2), Frequency=c(0.2,0.5,0.3))
  loci <- make_loci( f ) 
  
  expect_is(loci,"locus")
  expect_equal(length(loci),20)
  
  
  loci <- make_loci( f, F=1.0 )
  expect_equal( sum(is_heterozygote(loci)), 0)
  
  loci <- make_loci( f, F=0.5) 
  expect_equal( sum( is_heterozygote( loci )), 6)
  
})
