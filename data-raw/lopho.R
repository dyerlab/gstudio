# Rebuilds data/lopho.rda: the Lophocereus schottii Population Graph of
# Dyer & Nason (2004, Molecular Ecology 13:1713-1727), 21 populations
# (12 peninsular Baja California, 9 continental Sonora/Arizona) from the
# allozyme data of Nason et al. (2002).
#
# Edges are the 50 standardized inverse correlations r_ij marked significant
# (bold) in Table 1 of Dyer & Nason (2004); edge weights are the
# corresponding multivariate statistical distances d_ij from the same table.
# Node sizes and regions are carried over from the earlier example graph.
#
# Run from the package root: source("data-raw/lopho.R")

library( igraph )

edges <- matrix( c(
  "BaC", "LaV", 0.15, 9.04,
  "BaC", "Lig", 0.07, 9.75,
  "BaC", "PtP", 0.10, 6.46,
  "BaC", "SenBas", 0.07, 7.67,
  "BaC", "SnE", 0.11, 7.76,
  "BaC", "SnI", 0.07, 6.88,
  "BaC", "StR", 0.23, 6.64,
  "CP", "LF", 0.07, 4.29,
  "CP", "Seri", 0.41, 2.67,
  "CP", "SG", 0.21, 3.70,
  "CP", "SN", 0.15, 4.08,
  "CP", "TS", 0.20, 4.36,
  "Ctv", "PtP", 0.25, 2.65,
  "Ctv", "SenBas", 0.10, 5.73,
  "Ctv", "SLG", 0.64, 1.39,
  "Ctv", "SnF", 0.20, 2.70,
  "LaV", "Lig", 0.08, 12.18,
  "LaV", "SnE", 0.23, 8.44,
  "LaV", "SnF", 0.09, 10.45,
  "LaV", "TsS", 0.38, 7.48,
  "LF", "PL", 0.39, 2.46,
  "LF", "SG", 0.28, 2.86,
  "LF", "SI", 0.17, 3.19,
  "Lig", "PtC", 0.09, 12.88,
  "Lig", "SnE", 0.06, 10.81,
  "Lig", "SnI", 0.10, 9.31,
  "Lig", "StR", 0.19, 9.25,
  "PL", "SenBas", 0.07, 8.87,
  "PL", "SG", 0.15, 3.60,
  "PL", "SI", 0.34, 2.95,
  "PtC", "SnE", 0.09, 10.47,
  "PtC", "StR", 0.12, 10.06,
  "PtC", "TsS", 0.49, 6.91,
  "PtP", "SenBas", 0.11, 5.82,
  "PtP", "SnF", 0.30, 2.99,
  "PtP", "SnI", 0.15, 4.45,
  "SenBas", "StR", 0.07, 8.63,
  "Seri", "SG", 0.19, 3.46,
  "Seri", "SN", 0.25, 3.57,
  "SG", "SI", 0.13, 3.91,
  "SI", "SN", 0.07, 4.96,
  "SI", "TS", 0.22, 4.34,
  "SLG", "SnF", 0.18, 3.01,
  "SLG", "SnI", 0.10, 4.61,
  "SN", "TS", 0.17, 4.60,
  "SnE", "StR", 0.19, 7.62,
  "SnE", "TsS", 0.09, 9.11,
  "SnF", "SnI", 0.10, 4.86,
  "SnI", "StR", 0.09, 7.74,
  "StR", "TsS", 0.08, 9.31 ), ncol = 4, byrow = TRUE )

nodes <- data.frame(
  name   = c( "BaC", "Ctv", "LaV", "Lig", "PtC", "PtP", "SLG", "SnE", "SnF", "SnI", "StR",
              "TsS", "CP", "LF", "PL", "SenBas", "Seri", "SG", "SI", "SN", "TS" ),
  size   = c( 12.15881, 3.880886, 3.533757, 4.731355, 4.684652, 10.925375, 5.955645,
              11.82822, 6.325655, 5.466695, 6.859545, 5.29057, 7.870725, 8.472215,
              6.692795, 9.116705, 2.5, 11.02753, 11.52145, 8.325785, 16.001165 ),
  region = rep( c( "Baja", "Sonora" ), c( 12, 9 ) ) )

lopho <- graph_from_data_frame( data.frame( from = edges[, 1], to = edges[, 2],
                                            weight = as.numeric( edges[, 4] ) ),
                                directed = FALSE, vertices = nodes )
class( lopho ) <- c( "popgraph", class( lopho ) )

stopifnot( vcount( lopho ) == 21, ecount( lopho ) == 50, components( lopho )$no == 1 )
save( lopho, file = "data/lopho.rda", compress = "xz" )
