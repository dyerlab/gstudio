context("partition.R")

test_that("checking",{
  df <- data.frame( Pop=rep(c("A","B"),each=4), VAL=1:8 )
  subs <- partition( df, stratum="Pop")
  
  expect_that( subs, is_a("list") )
  expect_that( names(subs), is_equivalent_to( c("A","B") ) )
  expect_that( length(subs), equals(2) )
  expect_that( subs[[1]], is_a("data.frame" ) )
  expect_that( subs[[2]], is_a("data.frame" ) )
  expect_that( length( subs[[1]]$Pop), equals(4) )
  expect_that( length( subs[[2]]$Pop), equals(4) )
})