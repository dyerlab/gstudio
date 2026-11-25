# Tests for gravity_surface(), and its use by plot.gravity_field().

# A chain of K populations on the diagonal of a lon/lat box.
surface_field <- function( K = 8 ) {
  nm <- paste0( "P", seq_len( K ) )
  A  <- matrix( 0, K, K, dimnames = list( nm, nm ) )
  for( i in seq_len( K - 1 ) )
    A[i, i + 1] <- A[i + 1, i] <- 1 + ( i %% 3 ) / 2
  g <- igraph::graph_from_adjacency_matrix( A, mode = "undirected", weighted = TRUE )
  igraph::V( g )$Longitude <- seq( -113, -111, length.out = K )
  igraph::V( g )$Latitude  <- 26 + sin( seq_len( K ) )
  gravity_field( g )
}

# A component whose hull is a triangle, and a pair just below its lower side.
two_components <- function() {
  nm <- c( "A", "B", "C", "Y", "Z" )
  A  <- matrix( 0, 5, 5, dimnames = list( nm, nm ) )
  A["A", "B"] <- A["B", "A"] <- 1
  A["B", "C"] <- A["C", "B"] <- 2
  A["Y", "Z"] <- A["Z", "Y"] <- 1
  g  <- igraph::graph_from_adjacency_matrix( A, mode = "undirected", weighted = TRUE )
  xy <- cbind( x = c( 0, 5, 10, 4.5, 5.5 ), y = c( 0, 8, 0, -0.5, -0.5 ) )
  rownames( xy ) <- nm
  list( field = gravity_field( g ), xy = xy )
}

box <- function( xmin, xmax, ymin, ymax, crs = 4326 ) {
  sf::st_as_sfc( sf::st_bbox( c( xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax ),
                              crs = sf::st_crs( crs ) ) )
}

plotted_surface <- function( p ) {
  is_raster <- vapply( p$layers, function( l ) inherits( l$geom, "GeomRaster" ), logical( 1 ) )
  p$layers[[which( is_raster )]]$data
}

test_that( "gravity_surface returns a complete regular grid", {
  f <- surface_field()
  s <- gravity_surface( f, grid_n = 40 )
  expect_s3_class( s, "data.frame" )
  expect_named( s, c( "panel", "x", "y", "component", "S" ) )
  expect_equal( nrow( s ), 40^2 )
  expect_equal( is.na( s$S ), is.na( s$component ) )
  expect_true( any( !is.na( s$S ) ) && any( is.na( s$S ) ) )
  expect_lte( max( abs( s$S ), na.rm = TRUE ), max( abs( f$nodes$S ) ) + 1e-9 )
} )

test_that( "the hull is the convex hull of the discs around the nodes", {
  f  <- surface_field()
  xy <- cbind( f$nodes$Longitude, f$nodes$Latitude )
  r  <- 0.2
  s  <- gravity_surface( f, mask = "hull", mask_dist = r, grid_n = 60 )
  discs <- sf::st_buffer( sf::st_multipoint( xy ), r, nQuadSegs = 200 )
  hull  <- sf::st_convex_hull( discs )
  pts   <- sf::st_as_sf( s, coords = c( "x", "y" ) )
  inside <- lengths( sf::st_intersects( pts, hull ) ) > 0
  expect_equal( !is.na( s$S ), inside )
  # and mask = "nodes" is the same discs alone
  n <- gravity_surface( f, mask = "nodes", mask_dist = r, grid_n = 60 )
  expect_equal( !is.na( n$S ), lengths( sf::st_intersects( pts, discs ) ) > 0 )
} )

test_that( "the default mask is hulls, by coordinates and in graph space", {
  f <- surface_field()
  expect_equal( gravity_surface( f ), gravity_surface( f, mask = "hull" ) )
  kk <- gravity_surface( f, layout = "kk" )
  expect_equal( kk, gravity_surface( f, layout = "kk", mask = "hull" ) )
  discs <- gravity_surface( f, layout = "kk", mask = "nodes" )
  expect_true( all( is.na( discs$S ) | !is.na( kk$S ) ) )
  expect_gt( sum( !is.na( kk$S ) ), sum( !is.na( discs$S ) ) )
} )

test_that( "a graph-space surface is the same hull with and without the topology", {
  f  <- surface_field()
  kk <- gravity_surface( f, layout = "kk", grid_n = 40 )
  for( show in c( TRUE, FALSE ) ) {
    d <- plotted_surface( plot( f, layout = "kk", grid_n = 40, show_graph = show ) )
    expect_equal( d$value, kk$S )
  }
} )

test_that( "a component keeps the whole of its own hull", {
  d <- two_components()
  s <- gravity_surface( d$field, layout = d$xy, mask_dist = 1, grid_n = 121 )
  # Inside the triangle, though the nearest population is one of the pair.
  at <- function( x, y ) s[which.min( ( s$x - x )^2 + ( s$y - y )^2 ), ]
  expect_equal( at( 5, 0.4 )$component, 1 )
  expect_equal( at( 5, -0.5 )$component, 2 )
  expect_equal( at( 5, 4 )$component, 1 )
  # Two-node components have S = 0 and must not colour, or be coloured by, others.
  expect_equal( s$S[s$component %in% 2], rep( 0, sum( s$component %in% 2 ) ) )
} )

test_that( "the grid holds the whole buffer", {
  f <- surface_field()
  s <- gravity_surface( f, mask_dist = 3, grid_n = 80 )
  edge <- s$x %in% range( s$x ) | s$y %in% range( s$y )
  expect_true( all( is.na( s$S[edge] ) ) )
} )

test_that( "clip keeps the surface inside the polygons", {
  f    <- surface_field()
  land <- box( -114, -112, 24, 28 )
  s <- gravity_surface( f, grid_n = 60 )
  l <- gravity_surface( f, clip = land, grid_n = 60 )
  expect_equal( is.na( l$S ), is.na( s$S ) | s$x > -112 )
  # unclipped cells are unchanged
  expect_equal( l$S[!is.na( l$S )], s$S[!is.na( l$S )] )
  # sf, another CRS, and a base map give the same surface
  expect_equal( gravity_surface( f, clip = sf::st_sf( geometry = land ), grid_n = 60 ), l )
  # (short sides, so the straight edges of the box stay on it when projected)
  dense <- sf::st_set_crs( sf::st_segmentize( sf::st_set_crs( land, NA ), 0.05 ), 4326 )
  utm   <- sf::st_transform( dense, 32612 )
  u     <- gravity_surface( f, clip = utm, grid_n = 60 )
  expect_gt( mean( is.na( u$S ) == is.na( l$S ) ), 0.99 )
  base <- ggplot2::ggplot() + ggplot2::geom_sf( data = land )
  expect_equal( gravity_surface( f, clip = base, grid_n = 60 ), l )
} )

test_that( "a hole in the clip polygon is blank", {
  f     <- surface_field()
  lake  <- box( -112.2, -111.8, 25, 27 )
  shore <- sf::st_difference( sf::st_set_crs( box( -114, -110, 24, 28 ), NA ),
                              sf::st_set_crs( lake, NA ) )
  s <- gravity_surface( f, clip = shore, grid_n = 80 )
  in_lake <- s$x > -112.2 & s$x < -111.8 & s$y > 25 & s$y < 27
  expect_true( all( is.na( s$S[in_lake] ) ) )
  expect_true( any( !is.na( s$S[!in_lake] ) ) )
} )

test_that( "gravity_surface makes a SpatRaster that matches the table", {
  skip_if_not_installed( "terra" )
  f <- surface_field()
  s <- gravity_surface( f, grid_n = 50 )
  r <- gravity_surface( f, grid_n = 50, output = "raster" )
  expect_s4_class( r, "SpatRaster" )
  expect_equal( names( r ), "S" )
  expect_equal( terra::crs( r, describe = TRUE )$code, "4326" )
  v <- terra::extract( r, as.matrix( s[c( "x", "y" )] ) )[, 1]
  expect_equal( replace( v, is.nan( v ), NA ), s$S )
  # one layer per panel, and no CRS for a layout that is not geographic
  f2 <- gravity_field( f$graphs[[1]], gamma = c( 1, 0.5 ) )
  r2 <- gravity_surface( f2, layout = "kk", grid_n = 50, output = "raster" )
  expect_equal( names( r2 ), c( "gamma = 1", "gamma = 0.5" ) )
  expect_equal( terra::crs( r2 ), "" )
} )

test_that( "plot draws the surface of gravity_surface", {
  f <- surface_field()
  land <- box( -114, -112, 24, 28 )
  base <- ggplot2::ggplot() + ggplot2::geom_sf( data = land )
  p <- plot( f, base = base, clip_to_base = TRUE, grid_n = 60 )
  expect_equal( plotted_surface( p )$value,
                gravity_surface( f, clip = land, grid_n = 60 )$S )
  q <- plot( f, layout = "kk", grid_n = 60 )
  expect_equal( plotted_surface( q )$value,
                gravity_surface( f, layout = "kk", grid_n = 60 )$S )
  # components that cross no contour level draw without warnings
  d <- two_components()
  expect_no_warning( ggplot2::ggplot_build( plot( d$field, layout = d$xy ) ) )
} )

test_that( "gravity_surface checks its input", {
  f <- surface_field()
  expect_error( gravity_surface( f$graphs[[1]] ), "gravity_field" )
  expect_error( gravity_surface( f, layout = "kk", clip = box( -114, -112, 24, 28 ) ),
                "placed by coordinates" )
  expect_error( gravity_surface( f, clip = "land" ), "sf or sfc" )
  expect_error( gravity_surface( f, clip = sf::st_sfc( sf::st_point( c( 0, 0 ) ) ) ),
                "no polygons" )
  expect_error( gravity_surface( f, grid_n = 1 ), "grid_n" )
  expect_error( gravity_surface( f, mask_dist = -1 ), "mask_dist" )
} )

test_that( "plot draws a boundary where two components meet", {
  boundary_layer <- function( p ) {
    is_path <- vapply( p$layers,
                       function( l ) identical( class( l$geom )[1], "GeomPath" ),
                       logical( 1 ) )
    p$layers[is_path]
  }
  # Two pairs on facing vertical lines: the hulls meet along x = 0.
  nm <- c( "A", "B", "Y", "Z" )
  A  <- matrix( 0, 4, 4, dimnames = list( nm, nm ) )
  A["A", "B"] <- A["B", "A"] <- A["Y", "Z"] <- A["Z", "Y"] <- 1
  g  <- igraph::graph_from_adjacency_matrix( A, mode = "undirected", weighted = TRUE )
  xy <- cbind( x = c( -1, -1, 1, 1 ), y = c( -3, 3, -3, 3 ) )
  rownames( xy ) <- nm
  f  <- gravity_field( g )
  b  <- boundary_layer( plot( f, layout = xy, mask_dist = 2 ) )
  expect_length( b, 1 )
  expect_lt( max( abs( b[[1]]$data$x ) ), 0.05 )
  expect_equal( range( b[[1]]$data$y ), c( -1, 1 ) * ( 3 + sqrt( 3 ) ), tolerance = 0.05 )
  # drawn in every panel where they meet, and on voronoi surfaces
  f2 <- gravity_field( g, gamma = c( 1, 0.5 ) )
  b2 <- boundary_layer( plot( f2, layout = xy, mask_dist = 2, surface = "voronoi" ) )
  expect_equal( nlevels( droplevels( b2[[1]]$data$panel ) ), 2 )
  # none when they do not meet, with one component, or when turned off
  expect_length( boundary_layer( plot( f, layout = xy, mask_dist = 0.5 ) ), 0 )
  expect_length( boundary_layer( plot( surface_field() ) ), 0 )
  expect_length( boundary_layer( plot( f, layout = xy, mask_dist = 2, boundary = FALSE ) ), 0 )
  expect_error( plot( f, layout = xy, boundary = NA ), "TRUE or FALSE" )
  expect_no_warning( ggplot2::ggplot_build( plot( f2, layout = xy, mask_dist = 2 ) ) )
} )

test_that( "clip polygons that miss the surface give one clear warning", {
  f    <- surface_field()
  far  <- box( 0, 1, 0, 1 )
  base <- ggplot2::ggplot() + ggplot2::geom_sf( data = far ) + ggplot2::coord_sf()
  expect_warning( s <- gravity_surface( f, clip = far, grid_n = 30 ), "do not overlap" )
  expect_true( all( is.na( s$S ) ) )
  for( show in c( TRUE, FALSE ) ) {
    w <- capture_warnings( {
      p <- plot( f, base = base, clip_to_base = TRUE, grid_n = 30, show_graph = show )
      ggplot2::ggplotGrob( p )
    } )
    expect_length( w, 1 )
    expect_match( w, "do not overlap" )
  }
  expect_no_warning( gravity_surface( f, clip = box( -114, -112, 24, 28 ), grid_n = 30 ) )
} )

test_that( ".in_rings finds points in features with holes, parts and overlaps", {
  ring <- function( x, y ) data.frame( x = x, y = y )
  sq   <- function( a, b ) ring( c( a, b, b, a ), c( a, a, b, b ) )
  g    <- expand.grid( x = seq( -0.5, 10.5, by = 0.37 ), y = seq( -0.5, 10.5, by = 0.41 ) )
  in_sq <- function( a, b ) g$x > a & g$x < b & g$y > a & g$y < b
  # a hole, a second part, and a second feature over the hole
  f1 <- list( sq( 0, 8 ), sq( 2, 6 ), sq( 9, 10 ) )
  one <- ( in_sq( 0, 8 ) & !in_sq( 2, 6 ) ) | in_sq( 9, 10 )
  expect_equal( .in_rings( g$x, g$y, list( f1 ) ), one )
  expect_equal( .in_rings( g$x, g$y, list( f1, list( sq( 3, 5 ) ) ) ), one | in_sq( 3, 5 ) )
  # closed rings are the same, and points need not be on a grid or in order
  closed <- lapply( f1, function( r ) r[c( seq_len( nrow( r ) ), 1 ), ] )
  expect_equal( .in_rings( g$x, g$y, list( closed ) ), one )
  set.seed( 1 )
  o <- sample( nrow( g ) )
  expect_equal( .in_rings( g$x[o], g$y[o], list( f1 ) ), one[o] )
  # rings that cannot hold a point, incomplete rings, no rings and no points
  expect_false( any( .in_rings( g$x, g$y, list( list( sq( 20, 30 ) ) ) ) ) )
  expect_false( any( .in_rings( g$x, g$y, list( list( ring( c( 0, 1 ), c( 0, 1 ) ) ), list() ) ) ) )
  expect_identical( .in_rings( numeric(), numeric(), list( f1 ) ), logical() )
} )
