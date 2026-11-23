

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
  expect_true( d[1,2] == d[2,1] )
  expect_that( sum(diag(d)), equals(0))
  expect_true( ((d[1,2]-3793.538) < 0.02) )
  
  d <- strata_distance( df, mode="Euclidean")
  expect_true( ((d[1,2]-46.07806) < 1e-7)  )
  
})


test_that("strata_distance() takes raw data and finds the coordinates", {
  data(arapat)
  expect_equal(strata_distance(arapat), strata_distance(strata_coordinates(arapat)))
  expect_equal(strata_distance(arapat, stratum = "Species", mode = "Euclidean"),
               strata_distance(strata_coordinates(arapat, stratum = "Species"), mode = "Euclidean"))
  expect_equal(strata_distance(dplyr::group_by(arapat, Species)),
               strata_distance(strata_coordinates(arapat, stratum = "Species")))
  data(cornus_florida)
  a <- cornus_florida[cornus_florida$OffID == 0, ]
  D <- strata_distance(a, stratum = "ID", longitude = "X", latitude = "Y", mode = "Euclidean")
  expect_equal(dim(D), c(nrow(a), nrow(a)))
  expect_equal(D[1, 2], sqrt((a$X[1] - a$X[2])^2 + (a$Y[1] - a$Y[2])^2))
  expect_error(strata_distance(a), "cannot find column 'Population'")
})
