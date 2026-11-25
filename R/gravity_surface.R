#' Source-sink surface of a gravity field
#'
#' @description
#' Interpolates the source-sink score \eqn{S} of a \code{\link{gravity_field}}
#' onto a regular grid: the surface that \code{\link{plot.gravity_field}} draws,
#' as a table or a raster to map, export or analyse.  The surface of each
#' connected component fills the convex hull of that component (with a margin
#' around its outer populations); on a map, \code{clip} removes the parts of the
#' landscape where the species cannot be, such as water.
#'
#' @details
#' \eqn{S} sums to zero within a connected component, so scores are comparable
#' only within it and each component is interpolated from its own populations.
#' A cell within the reach of several components belongs to the one whose hull
#' is nearest, and, where hulls overlap, to the one with the nearest
#' population.
#'
#' The margin, \code{mask_dist}, is the radius of a disc around each population.
#' With \code{mask = "nodes"} the surface is those discs alone.  With
#' \code{mask = "hull"} it is the convex hull of the discs of each component, so
#' it uses the same radius and also fills the ground between populations.
#' The surface there is interpolated, and no population was sampled in it; when
#' the populations are placed by a graph layout, it is not a place at all.
#'
#' Distances are measured in the units of the layout.  For longitude and
#' latitude these are degrees.
#'
#' @param x A \code{\link{gravity_field}}.
#' @param layout Where the populations are; see \code{\link{plot.popgraph}}.  By
#'   default the \code{x}, \code{y} or \code{Longitude}, \code{Latitude} vertex
#'   attributes of the graph.
#' @param mask Where the surface is defined.  \code{"hull"} (default) fills the
#'   convex hull of each connected component, and \code{"nodes"} keeps a disc
#'   around each population.
#' @param mask_dist Radius of the disc around each population, in the units of
#'   the layout; default 1.2 times the median nearest-neighbour distance between
#'   populations (floored at a quarter of their mean spacing, so near-coincident
#'   populations do not blank the surface).
#' @param clip Where the surface may be: an \code{sf} or \code{sfc} object of
#'   polygons (e.g. land), or a \code{ggplot} whose \code{geom_sf()} and
#'   \code{geom_polygon()} layers are such polygons.  Cells whose centre is
#'   outside all of them are blank.  Needs populations placed by coordinates.
#'   Polygons are transformed to longitude and latitude when the layout is
#'   geographic; otherwise they must already be in the layout's coordinates.
#' @param surface \code{"interpolate"} (smooth Gaussian kernel interpolation of
#'   \eqn{S}) or \code{"voronoi"} (each cell takes the \eqn{S} of its nearest
#'   population in its component).
#' @param grid_n Number of cells along each axis (default 140).  A finer grid
#'   follows a clipped coastline more closely.
#' @param output \code{"data.frame"} (default) or \code{"raster"}, a
#'   \code{SpatRaster} (needs the \code{terra} package).
#' @return For \code{output = "data.frame"}, a \code{data.frame} with one row per
#'   cell and panel: \code{panel}, \code{x} and \code{y} (the cell centre),
#'   \code{component} (the connected component the cell belongs to) and
#'   \code{S}.  Cells outside the surface have \code{NA} for \code{component} and
#'   \code{S}, so the grid is complete.  For \code{output = "raster"}, a
#'   \code{terra::SpatRaster} of \eqn{S} with one layer per panel, named by panel
#'   (\code{"S"} for a single unnamed one), in longitude and latitude (EPSG:4326)
#'   when the layout is geographic and with no coordinate reference system
#'   otherwise.
#' @seealso \code{\link{gravity_field}}, \code{\link{plot.gravity_field}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' library( igraph )
#' A <- matrix( 0, 5, 5, dimnames = list( LETTERS[1:5], LETTERS[1:5] ) )
#' A["A", "B"] <- A["B", "A"] <- 1
#' A["B", "C"] <- A["C", "B"] <- 1.5
#' A["C", "D"] <- A["D", "C"] <- 0.8
#' A["D", "E"] <- A["E", "D"] <- 2
#' g <- graph_from_adjacency_matrix( A, mode = "undirected", weighted = TRUE )
#' V( g )$Longitude <- c( -113, -112.6, -112, -111.5, -111.2 )
#' V( g )$Latitude  <- c( 28, 27.2, 26.6, 26.4, 25.5 )
#' f <- gravity_field( g )
#'
#' s <- gravity_surface( f, grid_n = 60 )
#' head( s[!is.na( s$S ), ] )
#'
#' # Keep the surface on land, here everything south of 27.5 N
#' land <- sf::st_as_sfc( sf::st_bbox( c( xmin = -114, ymin = 24,
#'                                        xmax = -110, ymax = 27.5 ),
#'                                     crs = sf::st_crs( 4326 ) ) )
#' on_land <- gravity_surface( f, clip = land, grid_n = 60 )
#' c( all = sum( !is.na( s$S ) ), land = sum( !is.na( on_land$S ) ) )
#'
#' library( ggplot2 )
#' ggplot( on_land, aes( x, y, fill = S ) ) +
#'   geom_raster( na.rm = TRUE ) +
#'   scale_fill_gradient2( na.value = NA ) +
#'   coord_quickmap()
#'
#' # As a raster, e.g. for terra::writeRaster()
#' if( requireNamespace( "terra", quietly = TRUE ) )
#'   gravity_surface( f, clip = land, grid_n = 60, output = "raster" )
#' @export
gravity_surface <- function( x,
                             layout = NULL,
                             mask = c( "hull", "nodes" ),
                             mask_dist = NULL,
                             clip = NULL,
                             surface = c( "interpolate", "voronoi" ),
                             grid_n = 140,
                             output = c( "data.frame", "raster" ) ) {
  if( !inherits( x, "gravity_field" ) )
    stop( "'x' must be a gravity_field; make one with gravity_field( graph )." )
  mask    <- match.arg( mask )
  surface <- match.arg( surface )
  output  <- match.arg( output )
  if( output == "raster" && !requireNamespace( "terra", quietly = TRUE ) )
    stop( "output = \"raster\" needs the 'terra' package." )
  .check_surface_grid( grid_n, mask_dist )

  parts  <- .split_gravity_field( x )
  placed <- .field_layouts( parts, layout )
  panels <- x$diagnostics$panel
  nd <- do.call( rbind, lapply( seq_along( parts ), function( i ) {
    n  <- parts[[i]]$nodes
    xy <- placed$xy[[i]]
    data.frame( panel = panels[i],
                x = xy[n$Stratum, 1],
                y = xy[n$Stratum, 2],
                S = n$S,
                component = .node_component( parts[[i]] ),
                row.names = NULL )
  } ) )

  if( !is.null( clip ) ) {
    if( !placed$coordinates )
      stop( "'clip' needs populations placed by coordinates, not by a graph layout." )
    clip <- .clip_polygons( clip, placed$geographic )
  }
  out <- .surface_grid( nd, "S", panels, surface, mask_dist, grid_n,
                        mask = mask,
                        clip = clip )
  names( out )[names( out ) == "value"] <- "S"
  out <- out[c( "panel", "x", "y", "component", "S" )]
  if( output == "raster" )
    return( .surface_raster( out, panels, grid_n, placed$geographic ) )
  out
}

# Node positions of every panel: one layout (from the first panel) when all the
# panels share their populations, so facets are comparable.  'coordinates' is
# TRUE when every panel is placed by coordinates, rather than by a named or
# function layout; 'geographic' when those are longitude and latitude.
#' @keywords internal
#' @noRd
.field_layouts <- function( parts, layout ) {
  coords_for <- function( p ) {
    .graph_layout( p$graph,
                   layout,
                   layout_graph = dcgd( p$graph,
                                        gamma = p$diagnostics$gamma,
                                        output = "graph" ),
                   nodes = p$nodes$Stratum )
  }
  first <- parts[[1]]$nodes$Stratum
  same  <- all( vapply( parts, function( p ) setequal( p$nodes$Stratum, first ), logical( 1 ) ) )
  xy <- if( same ) rep( list( coords_for( parts[[1]] ) ), length( parts ) )
        else lapply( parts, coords_for )
  src <- vapply( xy, attr, character( 1 ), which = "source" )
  list( xy = unname( xy ),
        geographic = all( src == "geographic" ),
        coordinates = all( src != "layout" ) )
}

# Connected component of each row of a panel's node table.
#' @keywords internal
#' @noRd
.node_component <- function( p ) {
  memb <- igraph::components( p$graph )$membership
  as.integer( memb[match( p$nodes$Stratum, igraph::V( p$graph )$name )] )
}

#' @keywords internal
#' @noRd
.check_surface_grid <- function( grid_n, mask_dist ) {
  if( !is.numeric( grid_n ) || length( grid_n ) != 1 || !is.finite( grid_n ) || grid_n < 2 )
    stop( "'grid_n' must be a single number of at least 2." )
  if( !is.null( mask_dist ) &&
      ( !is.numeric( mask_dist ) || length( mask_dist ) != 1 ||
        !is.finite( mask_dist ) || mask_dist < 0 ) )
    stop( "'mask_dist' must be NULL or a single non-negative number." )
}

# Polygons to clip to, in the coordinates of the layout, as from
# .base_polygons().
#' @keywords internal
#' @noRd
.clip_polygons <- function( clip, geographic ) {
  if( ggplot2::is_ggplot( clip ) ) {
    out <- suppressWarnings( .base_polygons( clip ) )
  } else if( inherits( clip, c( "sf", "sfc" ) ) ) {
    g <- sf::st_geometry( clip )
    if( geographic && !is.na( sf::st_crs( g ) ) && !isTRUE( sf::st_is_longlat( g ) ) )
      g <- sf::st_transform( g, 4326 )
    out <- .sfc_polygons( g )
  } else {
    stop( "'clip' must be an sf or sfc object of polygons, or a ggplot with polygon layers." )
  }
  if( !length( out ) ) stop( "'clip' has no polygons." )
  out
}

# Features of an sfc: a list with one entry per (multi)polygon, each a list of
# rings (data.frames x, y).  Points and lines are dropped.
#' @keywords internal
#' @noRd
.sfc_polygons <- function( g ) {
  g <- g[!sf::st_is_empty( g )]
  g <- g[sf::st_dimension( g ) == 2]
  lapply( seq_along( g ), function( k ) {
    cc <- as.data.frame( sf::st_coordinates( g[k] ) )
    L  <- intersect( c( "L3", "L2", "L1" ), names( cc ) )
    xy <- stats::setNames( cc[c( "X", "Y" )], c( "x", "y" ) )
    if( length( L ) ) split( xy, interaction( cc[L], drop = TRUE ) ) else list( xy )
  } )
}

# The surface of nd[[value]] on a grid_n x grid_n grid, for each panel in
# 'lab': a data.frame x, y, component, value, panel.  Each cell is interpolated
# from the nodes of the one component it belongs to (all of them when nd has no
# 'component').  mask = "nodes" keeps cells within mask_dist of a node; "hull"
# keeps cells within mask_dist of the convex hull of a component.  'clip' (from
# .base_polygons()) further keeps only cells inside its polygons.
#' @keywords internal
#' @noRd
.surface_grid <- function( nd, value, lab, surface, mask_dist = NULL, grid_n = 140,
                           mask = "hull", clip = NULL ) {
  xy1  <- nd[nd$panel == lab[1], c( "x", "y" )]
  dmat <- as.matrix( stats::dist( xy1 ) )
  diag( dmat ) <- Inf
  nn <- stats::median( apply( dmat, 1, min ) )
  # Near-coincident nodes shrink the median nearest-neighbour distance toward
  # zero, which would mask almost the whole surface; floor it at a quarter of
  # the mean spacing of the nodes over their bounding box.
  if( !is.finite( nn ) ) nn <- 0
  box <- diff( range( xy1$x ) ) * diff( range( xy1$y ) )
  nn  <- max( nn, 0.25 * sqrt( box / nrow( xy1 ) ), 1e-9 )
  if( is.null( mask_dist ) ) mask_dist <- 1.2 * nn
  h <- max( nn * 1.2, 1e-6 )

  # The margin must hold the whole buffer, or the grid cuts it off flat.
  rx  <- range( nd$x )
  ry  <- range( nd$y )
  pad <- max( 0.12 * max( diff( rx ), diff( ry ), 1e-9 ), 1.05 * mask_dist )
  gx  <- seq( rx[1] - pad, rx[2] + pad, length.out = grid_n )
  gy  <- seq( ry[1] - pad, ry[2] + pad, length.out = grid_n )
  gr  <- expand.grid( x = gx, y = gy, KEEP.OUT.ATTRS = FALSE )
  on  <- if( is.null( clip ) ) rep( TRUE, nrow( gr ) ) else .in_rings( gr$x, gr$y, clip )

  out <- do.call( rbind, lapply( lab, function( l ) {
    n    <- nd[nd$panel == l, ]
    comp <- if( is.null( n$component ) ) rep( 1L, nrow( n ) ) else n$component
    sets <- split( seq_len( nrow( n ) ), comp )
    d2   <- outer( gr$x, n$x, "-" )^2 + outer( gr$y, n$y, "-" )^2
    # Squared distance from each cell to the nearest node of each component.
    near <- vapply( sets, function( k ) do.call( pmin, lapply( k, function( j ) d2[, j] ) ),
                    numeric( nrow( gr ) ) )
    if( mask == "hull" ) {
      reach <- vapply( sets,
                       function( k ) .hull_distance( gr$x, gr$y, n$x[k], n$y[k] ),
                       numeric( nrow( gr ) ) )
    } else {
      reach <- sqrt( near )
    }
    # The component that reaches the cell from the shortest distance; where
    # several hulls cover it, the one with the nearest node.
    rmin <- do.call( pmin, lapply( seq_along( sets ), function( k ) reach[, k] ) )
    cand <- near
    cand[reach > rmin + 1e-9] <- Inf
    own  <- max.col( -cand, ties.method = "first" )
    keep <- on & rmin <= mask_dist

    v <- rep( NA_real_, nrow( gr ) )
    for( k in seq_along( sets ) ) {
      cells <- which( keep & own == k )
      if( !length( cells ) ) next
      dk <- d2[cells, sets[[k]], drop = FALSE]
      z  <- n[[value]][sets[[k]]]
      if( surface == "voronoi" ) {
        v[cells] <- z[max.col( -dk, ties.method = "first" )]
      } else {
        # Weights relative to the component's nearest node, so cells far from
        # every node (inside a large hull) do not underflow to zero.
        w <- exp( -0.5 * ( dk - near[cells, k] ) / h^2 )
        v[cells] <- as.numeric( w %*% z ) / rowSums( w )
      }
    }
    id <- as.integer( names( sets ) )[own]
    id[!keep] <- NA
    data.frame( gr, component = id, value = v, panel = l )
  } ) )
  if( !is.null( clip ) && all( is.na( out$value ) ) )
    warning( "The clip polygons do not overlap the surface, so it is blank; check that they ",
             "cover the populations and use the same coordinates.", call. = FALSE )
  out
}

# Lines along which the cells of two components meet, for each panel of a
# .surface_grid() table: a data.frame x, y, line (one value per line), panel.
#' @keywords internal
#' @noRd
.surface_boundaries <- function( d ) {
  gx <- sort( unique( d$x ) )
  gy <- sort( unique( d$y ) )
  nx <- length( gx )
  ny <- length( gy )
  out <- list()
  for( p in unique( as.character( d$panel ) ) ) {
    m <- matrix( d$component[d$panel == p], nx, ny )
    # Pairs of components in cells that share a side.
    a <- c( m[-nx, ], m[, -ny] )
    b <- c( m[-1, ], m[, -1] )
    met   <- which( !is.na( a ) & !is.na( b ) & a != b )
    pairs <- unique( cbind( pmin( a[met], b[met] ), pmax( a[met], b[met] ) ) )
    for( i in seq_len( nrow( pairs ) ) ) {
      # 1 in one component, 0 in the other, smoothed so that the half-way
      # contour does not follow the steps of the cells.
      z <- ifelse( m %in% pairs[i, ], as.numeric( m == pairs[i, 1] ), NA )
      z <- .smooth_cells( matrix( z, nx, ny ), passes = 4 )
      for( l in grDevices::contourLines( gx, gy, z, levels = 0.5 ) )
        out[[length( out ) + 1]] <- data.frame( x = l$x,
                                                y = l$y,
                                                line = length( out ) + 1L,
                                                panel = p )
    }
  }
  if( !length( out ) )
    return( data.frame( x = numeric(), y = numeric(), line = integer(), panel = character() ) )
  out <- do.call( rbind, out )
  if( is.factor( d$panel ) ) out$panel <- factor( out$panel, levels( d$panel ) )
  out
}

# Repeated 3 x 3 means over the cells that are not NA, which stay NA.
#' @keywords internal
#' @noRd
.smooth_cells <- function( z, passes ) {
  sum3 <- function( m ) {
    m <- m + rbind( m[-1, ], 0 ) + rbind( 0, m[-nrow( m ), ] )
    m + cbind( m[, -1], 0 ) + cbind( 0, m[, -ncol( m )] )
  }
  ok <- !is.na( z )
  n  <- sum3( ok + 0 )
  for( i in seq_len( passes ) ) {
    z[!ok] <- 0
    z <- sum3( z ) / n
  }
  z[!ok] <- NA
  z
}

# A gravity_surface() table as a SpatRaster, one layer per panel.
#' @keywords internal
#' @noRd
.surface_raster <- function( d, panels, grid_n, geographic ) {
  gx <- sort( unique( d$x ) )
  gy <- sort( unique( d$y ) )
  hx <- diff( gx[1:2] ) / 2
  hy <- diff( gy[1:2] ) / 2
  # Raster rows run from north to south; the grid runs south to north.
  flip <- as.vector( matrix( seq_len( grid_n^2 ), nrow = grid_n )[, grid_n:1] )
  vals <- vapply( panels, function( p ) d$S[d$panel == p][flip], numeric( grid_n^2 ) )
  r <- terra::rast( nrows = grid_n,
                    ncols = grid_n,
                    nlyrs = length( panels ),
                    xmin = gx[1] - hx,
                    xmax = gx[grid_n] + hx,
                    ymin = gy[1] - hy,
                    ymax = gy[grid_n] + hy,
                    crs = if( geographic ) "EPSG:4326" else "" )
  terra::values( r ) <- vals
  names( r ) <- ifelse( nzchar( panels ), panels, "S" )
  r
}
