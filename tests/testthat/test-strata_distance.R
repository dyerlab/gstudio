context("strata_distance.R")


test_that("checking",{
  loci <- c( locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ),
             locus( c(1,1) ) )
  pops <- factor( rep( c("A","B"), each=4 ) )
  df <- data.frame( Populations=pops, loci=loci )
  
  expect_that( strata_distance(df), throws_error() )
  expect_that( strata_distance(), throws_error() )
  
  df <- data.frame(Stratum=0,Longitude=0,Latitude=0)
  expect_that( strata_diatnce(df, mode="Bob"), throws_error())
  
  # real data from 
  df <- data.frame( Stratum=c("SEA","RIC"), Longitude=c(-122.311778,-77.319667),Latitude=c(47.449889,37.505167) )
  
  d <- strata_distance(df)
  expect_that( d, is_a("matrix"))
  expect_that( dim(d), is_equivalent_to(c(2,2)))
  expect_that( d[1,2] == d[2,1], is_true() )
  expect_that( sum(diag(d)), equals(0))
  expect_that( ((d[1,2]-3793.538) < 0.02), is_true() )
  
  d <- strata_distance( df, mode="Euclidean")
  expect_that( ((d[1,2]-46.07806) < 1e-7) , is_true() )
  
})

