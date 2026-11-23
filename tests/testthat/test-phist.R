test_that("PhiST matches a direct AMOVA partition of the summed distances", {
  data(arapat)
  loci <- column_class( arapat, "locus" )
  missing <- rowSums( sapply( loci, function( l ) is.na( arapat[[ l ]] ) ) ) > 0
  cc <- arapat[ !missing, ]

  # Hand AMOVA (Excoffier et al. 1992) on distances summed across loci
  D <- genetic_distance( cc, mode = "AMOVA" )
  g <- as.character( cc$Species )
  N <- length( g ); n <- as.numeric( table( g ) ); K <- length( n )
  SS_total <- sum( D ) / ( 2 * N )
  SS_within <- sum( sapply( unique( g ), function( s ) {
    i <- which( g == s ); sum( D[ i, i ] ) / ( 2 * length( i ) ) } ) )
  MS_w <- SS_within / ( N - K )
  s2a <- ( ( SS_total - SS_within ) / ( K - 1 ) - MS_w ) / ( ( N - sum( n^2 ) / N ) / ( K - 1 ) )

  ps <- genetic_structure( cc, stratum = "Species", mode = "PhiST" )
  expect_s3_class( ps, "data.frame" )
  expect_equal( names( ps ), c( "Locus", "PhiST", "sigma2_among", "sigma2_within" ) )
  expect_equal( ps$Locus, c( loci, "Multilocus" ) )
  expect_equal( ps$PhiST[ nrow( ps ) ], s2a / ( s2a + MS_w ) )
  expect_equal( ps$sigma2_within[ nrow( ps ) ], MS_w )
  expect_null( ps$p.value )
})

test_that("PhiST handles missing data, permutations, case and pairwise", {
  data(arapat)
  x <- arapat[ arapat$Species == "Mainland", ]
  set.seed( 1 )
  ps <- genetic_structure( x, mode = "phist", nperm = 19 )
  expect_true( all( ps$p.value > 0 & ps$p.value <= 1, na.rm = TRUE ) )
  expect_false( is.na( ps$PhiST[ nrow( ps ) ] ) )
  expect_equal( ps$PhiST, genetic_structure( x, mode = "PhiST" )$PhiST )

  pw <- genetic_structure( x, mode = "PhiST", pairwise = TRUE )
  expect_true( is.matrix( pw ) )
  expect_true( all( is.na( diag( pw ) ) ) )
  expect_equal( pw, t( pw ) )

  one <- genetic_structure( x[ , c( "Population", "MP20" ) ], mode = "PhiST" )
  expect_equal( nrow( one ), 1 )
  expect_equal( one$Locus, "MP20" )
})

test_that("PhiST drops individuals with a missing stratum, also when permuting", {
  data(arapat)
  x <- arapat
  x$Population[ 1:20 ] <- NA
  set.seed( 2 )
  expect_no_warning( expect_message( ps <- genetic_structure( x, mode = "PhiST", nperm = 9 ), "20 individuals" ) )
  ref <- suppressMessages( genetic_structure( x[ -( 1:20 ), ], mode = "PhiST" ) )
  expect_equal( ps$PhiST, ref$PhiST )
  expect_true( all( ps$p.value > 0 & ps$p.value <= 1, na.rm = TRUE ) )
})
