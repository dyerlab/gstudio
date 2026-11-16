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
  expect_equal( names( ft ), c( "Locus", "PhiFT", "sigma2_among", "sigma2_within", "Nep", "p.value" ) )
  expect_equal( ft$Nep, 1 / ( 2 * ft$PhiFT ) )
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

test_that("dist_amova(pollen_pool = TRUE) treats every diploid as an ambiguous gamete", {
  # All gametes ambiguous: without the flag these would be diploid genotypes.
  l <- locus( c( "A:B", "A:B", "B:C" ), type = "separated" )
  p <- c( A = 2, B = 3, C = 1 ) / 6
  v <- rbind( c( p["A"], p["B"], 0 ) / ( p["A"] + p["B"] ), c( p["A"], p["B"], 0 ) / ( p["A"] + p["B"] ),
              c( 0, p["B"], p["C"] ) / ( p["B"] + p["C"] ) ) / 2
  expect_silent( D <- dist_amova( l, pollen_pool = TRUE ) )
  expect_equal( D, unname( as.matrix( stats::dist( v ) )^2 * 2 ), tolerance = 1e-8 )
  expect_false( isTRUE( all.equal( D, dist_amova( l ) ) ) )
  # Haploid-only gametes are the same either way.
  h <- locus( c( "A", "B", "C", "A" ), type = "separated" )
  expect_equal( dist_amova( h, pollen_pool = TRUE ), dist_amova( h ) )
})

.family_data <- function( seed = 5, n = 12 ) {
  set.seed( seed )
  moms <- c( "A:B", "C:D", "B:D", "A:C" )
  do.call( rbind, lapply( seq_along( moms ), function( k ) {
    ma <- strsplit( moms[k], ":" )[[1]]
    kids <- paste( sample( ma, n, replace = TRUE ), sample( c("A","B","C","D","E"), n, replace = TRUE ), sep = ":" )
    data.frame( ID = paste0( "m", k ), OffID = as.character( 0:n ), L = c( moms[k], kids ) )
  }))
}

test_that("missing maternal genotypes are reconstructed from offspring", {
  x <- .family_data()
  x$L <- locus( x$L, type = "separated" )
  full <- genetic_structure( x, mode = "PhiFT" )

  y <- x
  y$L[ y$ID == "m2" & y$OffID == "0" ] <- locus( "", type = "separated" )
  expect_message( ft <- genetic_structure( y, mode = "PhiFT" ), "m2 at L: inferred C:D" )
  rep <- attr( ft, "maternal_inference" )
  expect_equal( rep$Genotype, "C:D" )
  expect_true( rep$Used )
  expect_gte( rep$LR, 20 )
  expect_equal( ft$PhiFT, full$PhiFT )

  # Too few offspring: not inferred, and her offspring drop out at that locus.
  z <- y[ !( y$ID == "m2" & as.numeric( y$OffID ) > 1 ), ]
  expect_message( fz <- genetic_structure( z, mode = "PhiFT" ), "NOT inferred" )
  expect_false( attr( fz, "maternal_inference" )$Used )
  expect_equal( fz$PhiFT, genetic_structure( z[ z$ID != "m2", ], mode = "PhiFT" )$PhiFT )
})

test_that("pairwise PhiFT is a sub-analysis of the global gametes", {
  x <- .family_data()
  x$L <- locus( x$L, type = "separated" )
  pw <- expect_silent( genetic_structure( x, mode = "PhiFT", pairwise = TRUE ) )
  expect_equal( dim( pw ), c( 4, 4 ) )
  expect_equal( pw, t( pw ) )
  # Distances (and ambiguous-gamete frequencies) come from all families.
  off <- suppressMessages( minus_mom( x ) )
  D <- dist_amova( off$L, pollen_pool = TRUE )
  i <- which( off$ID %in% c( "m1", "m3" ) )
  expect_equal( pw[ "m1", "m3" ], .hand_phi( D[ i, i ], off$ID[ i ] ) )
})

test_that("mothers without offspring are not reconstructed", {
  x <- .family_data()
  x <- rbind( x, data.frame( ID = "m9", OffID = "0", L = "" ) )
  x$L <- locus( x$L, type = "separated" )
  expect_no_message( ft <- genetic_structure( x, mode = "PhiFT" ) )
  expect_null( attr( ft, "maternal_inference" ) )
})
