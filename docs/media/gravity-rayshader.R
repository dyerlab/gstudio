# gravity-rayshader.R
#
# A three-dimensional, ray-traced view of a source-sink surface, with the Population Graph
# and the direction of genetic gravity on each edge drawn above it.
#
# This script is an example of extending gstudio and is not part of the package. gstudio
# estimates the surface (gravity_surface()) and the directions (gravity_field()); everything
# else here is drawing.  It is described in the Genetic Gravity chapter of
# https://dyerlab.github.io/gstudio/
#
# Usage
#   Rscript gravity-rayshader.R [output file]      (default gravity-source-sink-3d.png)
#   or source( "gravity-rayshader.R" )             (the figure is then 'relief_map')
#   A file name ending in .pdf keeps the network, contours and labels as vectors.
#
# Packages, beyond gstudio and the ones it installs
#   install.packages( c( "rayshader", "terra", "png", "ggrepel", "rnaturalearth" ) )
#   install.packages( "rnaturalearthhires", repos = "https://ropensci.r-universe.dev" )
#
# Cost
#   About a minute and 9 GB of memory at the settings below, on an Apple M4 Max.  Memory
#   follows view$size, and time follows that and nx.  While choosing a view, use
#   size = c( 900, 750 ) and nx <- 300.
#
# Steps
#   1. Settings                     5. The render
#   2. Data and gravity field       6. The camera: points to pixels
#   3. The surface                  7. Layers: grid, contours, sites, edges, arrows
#   4. Colour and light             8. The figure
#
# To use your own data, replace step 2: it must leave
#   field   a gravity_field of one panel, whose graph has Longitude and Latitude
#   mapped  that graph (an igraph / popgraph)
#   land    sf polygons to keep the surface on land
#
# Rodney J. Dyer <https://github.com/dyerlab/gstudio>


# ---- 1. Settings -------------------------------------------------------------
#
# rgl.useNULL must be set before rayshader is loaded. It stops rgl from opening an OpenGL
# window, so this also runs where OpenGL or X11 is not available, such as recent versions of
# macOS or a server.

options( rgl.useNULL = TRUE )

needed  <- c( "gstudio", "dplyr", "ggplot2", "rayshader", "terra", "png", "ggrepel",
              "rnaturalearth", "rnaturalearthhires" )
have    <- vapply( needed, requireNamespace, logical( 1 ), quietly = TRUE )
if( !all( have ) )
  stop( "Please install: ", paste( needed[!have], collapse = ", " ) )

library( gstudio )
library( dplyr )
library( ggplot2 )
library( rayshader )

args <- commandArgs( trailingOnly = TRUE )
file <- if( length( args ) ) args[1] else "gravity-source-sink-3d.png"

nx           <- 700         # cells of the surface from west to east
relief       <- 0.14 * nx   # height of the largest |S|, in cells
lift         <- 0.04 * nx   # height of the network above the surface, in cells
grid_step    <- 25          # cells between the grid lines
contour_step <- 0.05        # interval of S between contours
view <- list( theta = -25,             # rotation of the scene, degrees
              phi = 42,                # height of the camera, degrees above the horizon
              zoom = 0.72,             # larger is farther; too small cuts off the shadow
              size = c( 1800, 1500 ) ) # render, in pixels


# ---- 2. Data and gravity field -----------------------------------------------
#
# Peninsular Araptus attenuatus, at sites with at least four beetles. Two southern components
# of two sites carry no directional information (S = 0 by symmetry), so only the larger
# components are mapped.

data( arapat )

beetles <- arapat |>
  filter( Species == "Peninsula" ) |>
  group_by( Population ) |>
  filter( n() >= 4 ) |>
  ungroup() |>
  droplevels() |>
  population_graph( decorate = TRUE )

parts  <- igraph::components( beetles )
mapped <- beetles |>
  igraph::induced_subgraph( which( parts$csize[parts$membership] > 2 ) )
field  <- mapped |>
  gravity_field()

land <- rnaturalearth::ne_countries( country = c( "Mexico", "United States of America" ),
                                     scale = "large",
                                     returnclass = "sf" )

nodes <- data.frame( Stratum = igraph::V( mapped )$name,
                     x = igraph::V( mapped )$Longitude,
                     y = igraph::V( mapped )$Latitude )


# ---- 3. The surface ----------------------------------------------------------
#
# rayshader takes cells to be square, and those of a longitude and latitude grid are not, so
# the surface is resampled to cells that are square on the ground. S is comparable only within
# a connected component, so each is resampled on its own and no cell blends the scores of two.

d    <- field |>
  gravity_surface( grid_n = nx, clip = land )
r    <- field |>
  gravity_surface( grid_n = nx, clip = land, output = "raster" )
comp <- r
terra::values( comp ) <- d$component[order( -d$y, d$x )]

extent <- as.vector( terra::ext( r ) )
wide   <- ( extent[2] - extent[1] ) * cospi( mean( extent[3:4] ) / 180 )
ny     <- round( nx * ( extent[4] - extent[3] ) / wide )
square <- terra::rast( terra::ext( r ),
                       nrows = ny,
                       ncols = nx,
                       crs = terra::crs( r ) )
comp_sq <- terra::resample( comp, square, method = "near" )
S_sq    <- terra::resample( r, square, method = "near" )
for( k in unique( na.omit( d$component ) ) ) {
  own  <- r |>
    terra::mask( comp, maskvalues = k, inverse = TRUE ) |>
    terra::resample( square, method = "bilinear" )
  S_sq <- terra::ifel( comp_sq == k & !is.na( own ), own, S_sq )
}

# Islands with no sampled site hold only extrapolated scores.
island <- terra::patches( S_sq, directions = 8 )
held   <- terra::extract( island, as.matrix( nodes[c( "x", "y" )] ) )
vals   <- terra::values( S_sq )
vals[!( terra::values( island ) %in% na.omit( held[, 1] ) )] <- NA
terra::values( S_sq ) <- vals

# rayshader's matrices: rows west to east, columns north to south.
as_matrix <- function( r ) {
  m <- matrix( terra::values( r ), terra::ncol( r ), terra::nrow( r ) )
  m[!is.finite( m )] <- NA
  m
}
S <- as_matrix( S_sq )
C <- as_matrix( comp_sq )
C[is.na( S )] <- NA
lim <- max( abs( S ), na.rm = TRUE )

# Height, with the land and sea around the surface at its lowest score.
z <- S / lim * relief
z <- z - min( z, na.rm = TRUE )
z[is.na( z )] <- 0


# ---- 4. Colour and light -----------------------------------------------------
#
# The texture is painted with the colours of the flat map: blue for sinks, red for sources. A
# soft light, from the slopes of S within each component, brings out the curvature without
# overwhelming the colours that carry the sign of S. Neither the walls at the edge of the
# surface nor the step between components is lit.

pal <- colorRampPalette( c( "#2166ac", "#f7f7f7", "#b2182b" ),
                         space = "Lab" )( 511 )

on_land <- terra::vect( land ) |>
  terra::rasterize( S_sq, field = 1 ) |>
  as_matrix()

# The value d cells away, or NA where that is in another component.
shifted <- function( m, dx, dy ) {
  i <- seq_len( nx ) + dx
  j <- seq_len( ny ) + dy
  i[i < 1 | i > nx] <- NA
  j[j < 1 | j > ny] <- NA
  m <- m[i, j]
  m[is.na( C ) | is.na( C[i, j] ) | C != C[i, j]] <- NA
  m
}

# Cells that touch another component.
meets <- matrix( FALSE, nx, ny )
for( dx in -1:1 ) for( dy in -1:1 ) {
  i <- pmin( nx, pmax( 1, seq_len( nx ) + dx ) )
  j <- pmin( ny, pmax( 1, seq_len( ny ) + dy ) )
  meets <- meets | ( !is.na( C ) & !is.na( C[i, j] ) & C != C[i, j] )
}

hex <- pal[round( ( S + lim ) / ( 2 * lim ) * 510 ) + 1]
hex[is.na( S ) & !is.na( on_land )] <- "#d9d9d9"
hex[is.na( S ) & is.na( on_land )]  <- "#eaf0f3"
hex[meets] <- "#404040"
texture <- array( t( col2rgb( hex ) ) / 255, c( nx, ny, 3 ) ) |>
  aperm( c( 2, 1, 3 ) )

soft_light <- function( amount = 0.22, azimuth = 285, altitude = 45 ) {
  Z <- S / lim * relief
  h <- 3
  slope <- function( a, b ) {
    s <- ( a - b ) / ( 2 * h )
    s <- ifelse( is.na( b ), ( a - Z ) / h, s )
    ifelse( is.na( a ), ( Z - b ) / h, s )
  }
  gx  <- slope( shifted( Z, h, 0 ), shifted( Z, -h, 0 ) )
  gy  <- slope( shifted( Z, 0, -h ), shifted( Z, 0, h ) )
  sun <- gx * sinpi( azimuth / 180 ) + gy * cospi( azimuth / 180 )
  up  <- sinpi( altitude / 180 )
  lit <- ( up - cospi( altitude / 180 ) * sun ) / sqrt( 1 + gx^2 + gy^2 )
  m   <- 1 + amount * tanh( ( lit / up - 1 ) / 0.6 )
  m[is.na( m )] <- 1
  m
}
light <- t( soft_light() )
for( b in 1:3 ) texture[, , b] <- pmin( 1, texture[, , b] * light )


# ---- 5. The render -----------------------------------------------------------
#
# ray_shade() traces the cast shadows, ambient_shade() darkens hollows, and render_snapshot(
# software_render = TRUE ) draws the shaded mesh without OpenGL.
#
# That renderer takes a texture to be in the linear ACEScg colour space, which shifts ordinary
# (sRGB) colours: #b2182b is drawn as #ff2071. Converting first makes the colours of the
# surface match those of the legend.

to_acescg <- function( img ) {
  img <- img[, , 1:3]     # without the alpha channel
  low <- img <= 0.04045
  lin <- ifelse( low, img / 12.92, ( ( img + 0.055 ) / 1.055 )^2.4 )
  M   <- rbind( c( 0.613097, 0.339523, 0.047379 ),
                c( 0.070194, 0.916354, 0.013452 ),
                c( 0.020616, 0.109570, 0.869815 ) )
  array( matrix( lin, ncol = 3 ) %*% t( M ), dim( img ) )
}

snapshot <- function( img, fsaa ) {
  plot_3d( to_acescg( img ),
           z,
           zscale = 1,
           theta = view$theta,
           phi = view$phi,
           zoom = view$zoom,
           windowsize = view$size,
           solidcolor = "grey80",
           solidlinecolor = NULL,
           soliddepth = -0.03 * nx,
           shadowdepth = -0.06 * nx,
           background = "white" )
  file <- tempfile( fileext = ".png" )
  render_snapshot( file,
                   software_render = TRUE,
                   fsaa = fsaa,
                   plot = FALSE )
  png::readPNG( file )[, , 1:3]
}

cast <- ray_shade( z, zscale = 1, sunangle = 315, sunaltitude = 50 )
img  <- texture |>
  add_shadow( cast, 0.8 ) |>
  add_shadow( ambient_shade( z, zscale = 1 ), 0.75 ) |>
  snapshot( fsaa = 2 )

edge <- c( img[1, , ], img[nrow( img ), , ], img[, 1, ], img[, ncol( img ), ] )
if( min( edge ) < 0.99 )
  warning( "The scene runs off the edge of the render; increase view$zoom." )


# ---- 6. The camera: points to pixels -----------------------------------------
#
# Lines and labels drawn by the renderer are pixelated, so they are drawn by ggplot2 over the
# image instead. That needs the pixel at which the camera puts any point. The view is
# orthographic, so a pixel is a linear function of the cell row, cell column and height. It is
# fitted to coloured dots, painted on the texture and found in renders of the same scene. (The
# network cannot be used for this: adding geometry to a scene may move its camera.)

r_dot <- 4
m     <- r_dot + 8

# Candidate dots on level ground, off the surface and on it.
candidates <- function( n_i, n_j ) {
  i <- round( seq( m, nx - m, length.out = n_i ) )
  j <- round( seq( m, ny - m, length.out = n_j ) )
  as.matrix( expand.grid( i = i, j = j ) )
}
level_at <- function( p, on_surface ) {
  i <- p[1] + -m:m
  j <- p[2] + -m:m
  rise <- diff( range( z[i, j] ) )
  if( on_surface ) !anyNA( S[i, j] ) && rise <= 14 else rise == 0
}
thin <- function( p, n ) {
  keep <- seq( 1, nrow( p ), length.out = min( n, nrow( p ) ) )
  p[unique( round( keep ) ), ]
}
off  <- candidates( 7, 9 )
on   <- candidates( 22, 28 )
off  <- off[apply( off, 1, level_at, on_surface = FALSE ), ]
on   <- on[apply( on, 1, level_at, on_surface = TRUE ), ]
dots <- rbind( thin( off, 12 ), thin( on, 24 ) )
dots <- dots[order( ( seq_len( nrow( dots ) ) - 1 ) %% 6 ), ]

# Six dots per render, told apart by colour.
hues  <- t( col2rgb( c( "red", "green", "blue",
                        "yellow", "magenta", "cyan" ) ) ) / 255
batch <- ( seq_len( nrow( dots ) ) - 1 ) %/% 6
found <- NULL
for( k in split( seq_along( batch ), batch ) ) {
  test <- array( 1, c( ny, nx, 3 ) )
  for( q in seq_along( k ) ) {
    i <- dots[k[q], "i"] + -r_dot:r_dot
    j <- dots[k[q], "j"] + -r_dot:r_dot
    for( b in 1:3 ) test[j, i, b] <- hues[q, b]
  }
  px <- snapshot( test, fsaa = 1 )
  for( q in seq_along( k ) ) {
    off_by <- abs( px[, , 1] - hues[q, 1] ) +
      abs( px[, , 2] - hues[q, 2] ) +
      abs( px[, , 3] - hues[q, 3] )
    hit <- which( off_by < 0.5, arr.ind = TRUE )
    if( nrow( hit ) >= 20 )
      found <- rbind( found, c( dots[k[q], ],
                                z = z[t( dots[k[q], ] )],
                                px = mean( hit[, 2] ),
                                py = mean( hit[, 1] ) ) )
  }
}

# Pixel = A %*% ( row, column, height, 1 ), less dots partly hidden.
X   <- cbind( found[, c( "i", "j", "z" )], 1 )
Y   <- found[, c( "px", "py" )]
use <- rep( TRUE, nrow( X ) )
for( pass in 1:5 ) {
  A   <- t( qr.solve( X[use, ], Y[use, ] ) )
  err <- sqrt( rowSums( ( X %*% t( A ) - Y )^2 ) )
  use <- err < max( 1.5, 3 * median( err ) )
}
message( sprintf( "camera: %d dots, median error %.2f px, largest %.2f px",
                  sum( use ), median( err[use] ), max( err[use] ) ) )

# Direction to the camera, per cell moved across the ground.
a <- A[1, 1:3]
b <- A[2, 1:3]
toward <- c( a[2] * b[3] - a[3] * b[2],
             a[3] * b[1] - a[1] * b[3],
             a[1] * b[2] - a[2] * b[1] )
toward <- toward / toward[3]
toward <- toward / sqrt( sum( toward[1:2]^2 ) )

# With the camera known, any point can be projected to its pixel.  It is hidden if the terrain
# rises above the line from it to the camera.  Visible stretches of a path share a 'run'.
project <- function( cell, pz, id = "" ) {
  xy      <- cbind( cell, pz, 1 ) %*% t( A )
  visible <- !is.na( pz )
  for( s in seq( 2, max( z ) / toward[3] + 2, by = 0.5 ) ) {
    i  <- round( cell[, 1] + s * toward[1] )
    j  <- round( cell[, 2] + s * toward[2] )
    ok <- which( visible & i >= 1 & i <= nx & j >= 1 & j <= ny )
    ray <- pz[ok] + s * toward[3] + 1
    visible[ok[z[cbind( i, j )[ok, , drop = FALSE]] > ray]] <- FALSE
  }
  runs <- rle( visible )
  data.frame( px = xy[, 1],
              py = -xy[, 2],
              visible = visible,
              run = paste( id, rep( seq_along( runs$lengths ),
                                    runs$lengths ) ) )
}

# Height of the surface at fractional cells, and NA off it.
surface_z <- function( cell ) {
  i  <- floor( cell[, 1] )
  j  <- floor( cell[, 2] )
  ok <- i >= 1 & i < nx & j >= 1 & j < ny
  i[!ok] <- 1
  j[!ok] <- 1
  f  <- cell[, 1] - i
  g  <- cell[, 2] - j
  zs <- ifelse( is.na( S ), NA, z )
  v  <- ( 1 - f ) * ( 1 - g ) * zs[cbind( i, j )] +
    f * ( 1 - g ) * zs[cbind( i + 1, j )] +
    ( 1 - f ) * g * zs[cbind( i, j + 1 )] +
    f * g * zs[cbind( i + 1, j + 1 )]
  v[!ok] <- NA
  v
}
drape <- function( cell, id ) project( cell, surface_z( cell ), id )


# ---- 7. Layers: grid, contours, sites, edges, arrows -------------------------
#
# The grid and contours are draped on the surface. The network floats above it: each edge runs
# from its source to its sink, along a straight line or, where the terrain is in the way, an
# arc just high enough to clear it. Drop lines tie the sites to their places.

# Grid lines.
row_line <- function( i ) {
  drape( cbind( i, seq( 1.5, ny - 0.5 ) ), paste( "row", i ) )
}
col_line <- function( j ) {
  drape( cbind( seq( 1.5, nx - 0.5 ), j ), paste( "col", j ) )
}
grid_lines <- rbind(
  do.call( rbind, lapply( seq( grid_step, nx, by = grid_step ), row_line ) ),
  do.call( rbind, lapply( seq( grid_step, ny, by = grid_step ), col_line ) )
)

# Contours, within each component.
n_level  <- ceiling( lim / contour_step )
level    <- seq( -n_level, n_level ) * contour_step
contours <- NULL
for( k in unique( na.omit( c( C ) ) ) ) {
  own <- S
  own[is.na( C ) | C != k] <- NA
  for( l in contourLines( seq_len( nx ),
                          seq_len( ny ),
                          own,
                          levels = level ) ) {
    id       <- paste( "contour", k, NROW( contours ) )
    line     <- drape( cbind( l$x, l$y ), id )
    contours <- rbind( contours,
                       cbind( line, zero = abs( l$level ) < 1e-9 ) )
  }
}

# Sites, at the height of the surface (the nearest cell on it).
cell <- cbind( ( nodes$x - extent[1] ) / diff( extent[1:2] ) * nx,
               ( extent[4] - nodes$y ) / diff( extent[3:4] ) * ny ) + 0.5
on   <- which( !is.na( S ), arr.ind = TRUE )
near <- apply( cell, 1, function( p ) {
  which.min( ( on[, 1] - p[1] )^2 + ( on[, 2] - p[2] )^2 )
} )
site_z <- z[on[near, ]]
sites  <- cbind( nodes, project( cell, site_z + lift ) )
feet   <- project( cell, site_z + 1 )
drops  <- do.call( rbind, lapply( seq_len( nrow( nodes ) ), function( k ) {
  up <- seq( 0, lift, length.out = 30 )
  project( cell[rep( k, 30 ), ], site_z[k] + up, paste( "drop", k ) )
} ) )

# Edges, from source to sink.
field_edges <- field$edges
edge_path <- function( k ) {
  ends  <- c( field_edges$source[k], field_edges$sink[k] )
  j     <- match( ends, nodes$Stratum )
  t     <- seq( 0, 1, length.out = 401 )
  at    <- cbind( cell[j[1], 1] + t * diff( cell[j, 1] ),
                  cell[j[1], 2] + t * diff( cell[j, 2] ) )
  chord <- site_z[j[1]] + t * diff( site_z[j] ) + lift
  arc   <- 4 * t * ( 1 - t )
  need  <- ( surface_z( at ) + lift / 2 - chord ) / arc
  rise  <- max( 0, need[arc > 0.05], na.rm = TRUE )
  cbind( project( at, chord + rise * arc, paste( "edge", k ) ),
         edge = k,
         t = t,
         delta = abs( field_edges$delta[k] ) )
}
edges <- do.call( rbind,
                  lapply( seq_len( nrow( field_edges ) ), edge_path ) )

# The part of the image that is not background: the whole of the scene and its shadow.
ink  <- apply( img, c( 1, 2 ), min ) < 0.999
pad  <- c( -10, 10 )
rows <- range( which( rowSums( ink ) > 0 ) ) + pad
cols <- range( which( colSums( ink ) > 0 ) ) + pad
mm   <- diff( cols ) / ( 7.8 * 25.4 )     # pixels per mm in the figure

d_max      <- max( abs( field_edges$delta ) )
edge_width <- function( d ) 0.2 + 0.55 * d / d_max

# Arrowheads sit at the middle of the edges and point to the sink.  |Delta| is shown by the
# width of the edge, not the length of an arrow as in plot(), because an oblique view shortens
# arrows by different amounts depending on their direction.  Heads are at most 0.4 of an edge.
arrowhead <- function( p ) {
  n   <- nrow( p )
  mid <- 201
  if( !p$visible[mid] || p$delta[1] < 1e-6 ) return( NULL )
  u   <- c( diff( p$px[mid + c( -20, 20 )] ),
            diff( p$py[mid + c( -20, 20 )] ) )
  u   <- u / sqrt( sum( u^2 ) )
  across <- sqrt( diff( p$px[c( 1, n )] )^2 + diff( p$py[c( 1, n )] )^2 )
  len    <- min( ( 1.6 + p$delta[1] / d_max ) * mm, 0.4 * across )
  along  <- len * c( 0.5, -0.5, -0.25, -0.5 )
  side   <- len * c( 0, 0.38, 0, -0.38 )
  data.frame( px = p$px[mid] + along * u[1] - side * u[2],
              py = p$py[mid] + along * u[2] + side * u[1],
              edge = p$edge[1] )
}
arrows <- do.call( rbind, lapply( split( edges, edges$edge ), arrowhead ) )


# ---- 8. The figure -----------------------------------------------------------
#
# Everything is drawn over the image. Blank labels keep the site names off the edges,
# arrowheads and drop lines.

shown <- function( d ) d[d$visible, ]
line  <- function( d, ... ) {
  geom_path( aes( px, py, group = run ),
             data = shown( d ),
             lineend = "round",
             ... )
}

heads <- aggregate( cbind( px, py ) ~ edge, arrows, mean )
every <- round( edges$t * 400 ) %% 10 == 5
blank <- rbind( shown( feet )[c( "px", "py" )],
                shown( edges[every, ] )[c( "px", "py" )],
                heads[c( "px", "py" )] )
labels <- rbind( data.frame( shown( sites )[c( "px", "py" )],
                             text = shown( sites )$Stratum,
                             size = 2.3 ),
                 data.frame( blank, text = "", size = 0.4 ) )

relief_map <- ggplot() +
  annotation_raster( img,
                     xmin = 0.5,
                     xmax = ncol( img ) + 0.5,
                     ymin = -nrow( img ) - 0.5,
                     ymax = -0.5,
                     interpolate = TRUE ) +
  geom_point( aes( x, y, fill = S ),
              data = data.frame( x = cols[1], y = -rows[1], S = 0 ),
              alpha = 0 ) +                     # for the colour bar
  line( grid_lines, colour = "grey15", alpha = 0.3, linewidth = 0.15 ) +
  line( contours[!contours$zero, ],
        colour = "grey15",
        alpha = 0.65,
        linewidth = 0.25 ) +
  line( contours[contours$zero, ],
        colour = "grey25",
        linewidth = 0.55 ) +
  line( drops, colour = "grey5", linewidth = 0.3 ) +
  geom_point( aes( px, py ),
              data = shown( feet ),
              size = 0.7,
              colour = "grey5" ) +
  geom_path( aes( px, py,
                  group = run,
                  linewidth = I( edge_width( delta ) + 0.4 ) ),
             data = shown( edges ),
             colour = "white",
             alpha = 0.7,
             lineend = "round" ) +
  geom_path( aes( px, py, group = run, linewidth = delta ),
             data = shown( edges ),
             lineend = "round" ) +
  geom_polygon( aes( px, py, group = edge ),
                data = arrows,
                fill = "black",
                colour = "white",
                linewidth = 0.25 ) +
  geom_point( aes( px, py ),
              data = shown( sites ),
              shape = 21,
              size = 2.3,
              stroke = 0.4,
              fill = "black",
              colour = "white" ) +
  ggrepel::geom_text_repel( aes( px, py,
                                 label = text,
                                 point.size = I( size ) ),
                            data = labels,
                            size = 1.9,
                            fontface = "bold",
                            bg.colour = "white",
                            bg.r = 0.08,
                            box.padding = 0.2,
                            point.padding = 0.15,
                            segment.size = 0.2,
                            min.segment.length = 0.25,
                            max.overlaps = Inf,
                            max.iter = 50000,
                            max.time = 5,
                            seed = 1 ) +
  scale_linewidth( name = expression( "|" * Delta *
                                        "| (arrow: source to sink)" ),
                   range = edge_width( c( 0, d_max ) ),
                   limits = c( 0, d_max ),
                   breaks = c( 0.25, 0.5, 0.75 ),
                   guide = guide_legend( title.position = "top",
                                         order = 2 ) ) +
  scale_fill_gradientn(
    name = "Source-sink score S (red = source, blue = sink)",
    colours = pal,
    limits = c( -lim, lim ),
    guide = guide_colourbar( title.position = "top", order = 1 )
  ) +
  coord_fixed( xlim = cols, ylim = -rev( rows ), expand = FALSE ) +
  theme_void( base_size = 11 ) +
  theme( legend.position = "bottom",
         legend.key.width = unit( 1.2, "cm" ),
         legend.key.height = unit( 0.3, "cm" ),
         legend.spacing.x = unit( 1, "cm" ),
         legend.background = element_blank(),
         legend.box.background = element_blank(),
         plot.margin = margin( 6, 6, 6, 6 ) )

ggsave( file,
        relief_map,
        width = 8,
        height = 8 * diff( rows ) / diff( cols ) + 0.9,
        dpi = 300,
        bg = "white" )
