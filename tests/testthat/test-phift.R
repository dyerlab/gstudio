# Hand AMOVA on a squared-distance matrix: returns PhiST
.hand_phi <- function( D, g ) {
  g <- as.character( g ); N <- length( g ); n <- as.numeric( table( g ) ); K <- length( n )
  SS_total <- sum( D ) / ( 2 * N )
  SS_within <- sum( sapply( unique( g ), function( s ) {
    i <- which( g == s ); sum( D[ i, i ] ) / ( 2 * length( i ) ) } ) )
  MS_w <- SS_within / ( N - K )
  s2a <- ( ( SS_total - SS_within ) / ( K - 1 ) - MS_w ) / ( ( N - sum( n^2 ) / N ) / ( K - 1 ) )
  s2a / ( s2a + MS_w )
}

test_that("PhiFT with unambiguous gametes is an AMOVA of the paternal alleles", {
  # Homozygous mothers, so every paternal allele is unambiguous.
  moms <- data.frame( ID = c("m1","m2","m3"), OffID = "0",
                      L = locus( c("A:A","B:B","C:C"), type = "separated" ) )
  pat <- c( "A","A","A","B",  "B","B","C","B",  "C","C","A","C" )
  mat <- rep( c("A","B","C"), each = 4 )
  offs <- data.frame( ID = rep( c("m1","m2","m3"), each = 4 ), OffID = as.character( rep( 1:4, 3 ) ),
                      L = locus( paste( pmin( mat, pat ), pmax( mat, pat ), sep = ":" ), type = "separated" ) )
  x <- rbind( moms, offs )
  ft <- genetic_structure( x, mode = "PhiFT" )
  expect_equal( names( ft ), c( "Locus", "PhiFT", "sigma2_among", "sigma2_within", "p.value" ) )
  # Gamete distance: 1 when the paternal alleles differ, 0 when they match.
  D <- outer( pat, pat, function( a, b ) as.numeric( a != b ) )
  expect_equal( ft$PhiFT, .hand_phi( D, offs$ID ) )
})

test_that("ambiguous gametes use their expected allele vector", {
  moms <- data.frame( ID = c("m1","m2"), OffID = "0",
                      L = locus( c("A:B","C:C"), type = "separated" ) )
  offs <- data.frame( ID = c("m1","m1","m1","m2","m2","m2"), OffID = as.character( 1:6 ),
                      L = locus( c("A:B","A:A","B:C","A:C","C:C","B:C"), type = "separated" ) )
  ft <- genetic_structure( rbind( moms, offs ), mode = "PhiFT" )
  # Gametes: ambiguous (A or B), A, C, A, C, B.  Frequencies at the locus come
  # from the remaining gametes (the ambiguous one counted with both alleles).
  p <- c( A = 3, B = 2, C = 2 ) / 7
  v <- rbind( c( p["A"], p["B"], 0 ) / ( p["A"] + p["B"] ), c(1,0,0), c(0,0,1), c(1,0,0), c(0,0,1), c(0,1,0) )
  D <- as.matrix( stats::dist( v ) )^2 / 2
  expect_equal( ft$PhiFT, .hand_phi( D, offs$ID ), tolerance = 1e-8 )
})

test_that("PhiFT detects structured pollen pools and drops incompatible gametes", {
  set.seed( 3 )
  alle <- c("A","B","C","D")
  fam <- lapply( 1:6, function( k ) {
    mom <- "E:E"
    pool <- if( k <= 3 ) c(0.85, 0.05, 0.05, 0.05) else c(0.05, 0.05, 0.05, 0.85)
    pat <- sample( alle, 15, replace = TRUE, prob = pool )
    data.frame( ID = paste0( "m", k ), OffID = as.character( 0:15 ),
                L = c( mom, paste0( pat, ":E" ) ) )
  })
  x <- do.call( rbind, fam )
  x$L <- locus( x$L, type = "separated" )
  ft <- genetic_structure( x, mode = "PhiFT", nperm = 99 )
  expect_gt( ft$PhiFT, 0.3 )
  expect_equal( ft$p.value, 0.01 )

  # An offspring that shares no allele with its mother is set to missing.
  x$L[2] <- locus( "A:A", type = "separated" )
  expect_message( genetic_structure( x, mode = "PhiFT" ), "incompatible" )

  # Pairwise among families, and an explicit stratum name.
  pw <- genetic_structure( x, mode = "PhiFT", pairwise = TRUE )
  expect_equal( dim( pw ), c( 6, 6 ) )
  names( x )[1] <- "Mother"
  expect_equal( genetic_structure( x, stratum = "Mother", mode = "phift" )$PhiFT,
                genetic_structure( setNames( x, c("ID","OffID","L") ), mode = "PhiFT" )$PhiFT )
})
