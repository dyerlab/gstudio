test_that("PhiST returns an amova object whose table matches a hand AMOVA", {
  data(arapat)
  loci <- column_class( arapat, "locus" )
  missing <- rowSums( sapply( loci, function( l ) is.na( arapat[[ l ]] ) ) ) > 0
  cc <- arapat[ !missing, ]
  fit <- genetic_structure( cc, stratum = "Species", mode = "PhiST" )
  expect_s3_class( fit, c( "amova", "data.frame" ), exact = TRUE )

  D <- genetic_distance( cc, mode = "AMOVA" )
  g <- as.character( cc$Species )
  N <- length( g ); K <- length( unique( g ) )
  SS_total <- sum( D ) / ( 2 * N )
  SS_within <- sum( sapply( unique( g ), function( s ) {
    i <- which( g == s ); sum( D[ i, i ] ) / ( 2 * length( i ) ) } ) )

  a <- anova( fit )
  expect_s3_class( a, "anova" )
  expect_equal( a$Df, c( K - 1, N - K, N - 1 ) )
  expect_equal( a$SS, c( SS_total - SS_within, SS_within, SS_total ) )
  expect_equal( a$MS[1:2], a$SS[1:2] / a$Df[1:2] )
  expect_equal( a$Sigma2[1:2], c( fit$sigma2_among[ nrow( fit ) ], fit$sigma2_within[ nrow( fit ) ] ) )
  expect_equal( sum( a$`% Var`[1:2] ), 100 )
  expect_equal( a$PhiST[1], fit$PhiST[ nrow( fit ) ] )

  out <- capture.output( print( fit ) )
  expect_true( any( grepl( "Analysis of Molecular Variance \\(PhiST\\)", out ) ) )
  expect_true( any( grepl( "Among strata", out ) ) )
  expect_true( any( grepl( "8 loci", out ) ) )
  expect_false( any( grepl( "Multilocus", out ) ) )

  df <- as.data.frame( fit )
  expect_identical( class( df ), "data.frame" )
  expect_null( attr( df, "amova" ) )
})

test_that("a single locus prints its own table, and missing data blank the multilocus Df", {
  data(arapat)
  one <- genetic_structure( arapat, mode = "PhiST", locus = "LTRS" )
  expect_true( any( grepl( "locus LTRS", capture.output( one ) ) ) )
  expect_false( is.na( anova( one )$Df[1] ) )

  set.seed( 4 )
  multi <- genetic_structure( arapat, mode = "PhiST", nperm = 9 )
  # arapat has missing genotypes, so N differs across loci
  expect_true( is.na( anova( multi )$Df[1] ) )
  out <- capture.output( multi )
  expect_true( any( grepl( "different numbers of individuals", out ) ) )
  expect_true( any( grepl( "9 permutations", out ) ) )
  expect_length( attr( multi, "permutations" ), 9 )
})

test_that("PhiFT returns an amova object with Nep", {
  data(cornus_florida)
  ft <- genetic_structure( cornus_florida, mode = "PhiFT" )
  expect_s3_class( ft, "amova" )
  out <- capture.output( ft )
  expect_true( any( grepl( "TwoGener", out ) ) )
  expect_true( any( grepl( "Among families", out ) ) )
  expect_true( any( grepl( "Nep", out ) ) )
})

test_that("plot shows the permutation distribution and the observed value", {
  data(arapat)
  set.seed( 7 )
  fit <- suppressMessages( genetic_structure( arapat, stratum = "Species", mode = "PhiST", nperm = 19 ) )
  p <- plot( fit )
  expect_s3_class( p, "ggplot" )
  b <- ggplot2::ggplot_build( p )
  expect_equal( sum( b$data[[1]]$count ), 19 )
  expect_equal( b$data[[2]]$xintercept, fit$PhiST[ nrow( fit ) ] )
  expect_error( plot( genetic_structure( arapat, stratum = "Species", mode = "PhiST" ) ), "nperm > 0" )
})
