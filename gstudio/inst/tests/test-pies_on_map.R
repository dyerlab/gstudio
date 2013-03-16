context("pies_on_map.R")


test_that("testing",{

  file <- system.file("data","arapat.rda",package="gstudio")
  load( file )
  
  expect_that( pies_on_map(FALSE), throws_error() )
  expect_that( pies_on_map(arapat), throws_error() )
  expect_that( pies_on_map(arapat,stratum="Population"), throws_error() )

  pies_on_map(arapat,stratum="Population",locus="EN")
  dev.off()

})
