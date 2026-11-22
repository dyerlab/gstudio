#' Sampling-design diagnostics for a Population Graph
#'
#' Reports whether a data set carries enough within-stratum information for a
#' well-estimated Population Graph and for the directional tests built on it.
#' A Population Graph's edges are estimated from among-stratum covariance
#' scaled by the within-stratum covariance, so the graph needs enough
#' individuals per stratum to estimate the within-stratum variance at each
#' locus.  For a biallelic or low-diversity locus that means roughly
#' \eqn{n_i H_{S,i} \gtrsim 5} expected heterozygotes per stratum; with fewer,
#' the graph tends to fragment and the source--sink score retains a degree
#' signature even at the degree-neutral bandwidth.  The power of the
#' directional tests rises with the number of strata and of independent loci.
#' The allocation that minimizes the variance of \eqn{\Phi}
#' (\code{\link{optimal_sampling}}, about \eqn{1/\Phi} individuals per stratum)
#' is reported for comparison: it is generally too thin for a Population Graph.
#'
#' The thresholds used for the flags come from simulations of a one-dimensional
#' stepping stone with 20 biallelic loci; they are guides, not tests.  For a
#' calibrated cutoff for a particular design use \code{\link{popgraph_null}}.
#'
#' @param x A \code{data.frame} of genotypes with \code{locus} columns and a
#'   stratum column.
#' @param stratum Name of the stratum column (default \code{"Population"}, or the
#'   grouping of a grouped data frame).
#' @param graph Optional Population Graph for \code{x} (default
#'   \code{population_graph(x, stratum)}).
#' @param gamma Bandwidth multiplier for the degree-share check (default 0.5).
#' @return An object of class \code{popgraph_diagnostics}: \code{design}
#'   (strata, individuals per stratum, loci, alleles, polymorphic loci,
#'   \eqn{G_{ST}}), \code{strata} (per stratum: \eqn{n_i}, mean \eqn{H_S}, and the
#'   median over loci of \eqn{n_i H_{S,il}}), \code{estimate} (the
#'   \code{"design"} attribute of the graph), \code{graph} (components,
#'   largest component, mean degree, leaves, degree share, \eqn{cor(S, S^0)}),
#'   \code{falconer} (the \code{optimal_sampling()} optimum for the total
#'   sample size and \eqn{G_{ST}}) and \code{flags}.
#' @seealso \code{\link{popgraph_null}}, \code{\link{optimal_sampling}},
#'   \code{\link{source_sink_scores}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' data(arapat)
#' popgraph_diagnostics(arapat)
#' @export
popgraph_diagnostics <- function(x, stratum = "Population", graph = NULL, gamma = 0.5) {
  if (!is.data.frame(x)) stop("'x' must be a data.frame")
  stratum <- .detect_stratum(x, stratum, default = "Population", explicit = !missing(stratum))
  x <- .plain_df(x)
  if (!(stratum %in% names(x))) stop(sprintf("cannot find the stratum column '%s'", stratum))
  loci <- column_class(x, "locus")
  grp <- factor(as.character(x[[stratum]])); strata <- levels(grp); K <- length(strata)
  if (is.null(graph)) graph <- population_graph(x, stratum = stratum)

  mv <- lapply(loci, function(l) { m <- to_mv(x[[l]]); m[rowSums(m) == 0, ] <- NA; m })
  fr <- lapply(mv, function(m) .pgn_freqs(m, grp))
  A <- vapply(mv, ncol, 1L)
  poly <- vapply(fr, function(f) any(colMeans(f$p, na.rm = TRUE) > 0 & colMeans(f$p, na.rm = TRUE) < 1), TRUE)
  n_i <- c(table(grp))
  he <- vapply(fr, function(f) (2 * f$n / pmax(2 * f$n - 1, 1)) * (1 - rowSums(f$p^2)), numeric(K))
  nh <- vapply(seq_along(fr), function(l) fr[[l]]$n * he[, l], numeric(K))
  gs <- .pgn_gst(fr[A > 1], n_i)
  sts <- data.frame(Stratum = strata, n = as.integer(n_i), Hs = rowMeans(he[, poly, drop = FALSE], na.rm = TRUE),
                    nHs = apply(nh[, poly, drop = FALSE], 1, stats::median, na.rm = TRUE), row.names = NULL)

  k <- igraph::degree(graph); cm <- igraph::components(graph)
  sc <- tryCatch(source_sink_scores(graph, gamma), error = function(e) NULL)
  gr <- c(edges = igraph::ecount(graph), components = cm$no, largest = max(cm$csize), mean_degree = mean(k),
          leaves = sum(k == 1), degree_share = if (is.null(sc)) NA else attr(sc, "degree_share"),
          cor_S_S0 = if (is.null(sc)) NA else attr(sc, "cor_S_S0"))
  N <- sum(n_i)
  fal <- tryCatch({ o <- optimal_sampling(N, max(gs[["gst"]], 1e-3)); b <- o[which.min(o$var_phi), ]
    c(strata = b$Stratum, per_stratum = N / b$Stratum) }, error = function(e) c(strata = NA, per_stratum = NA))

  flags <- character(0)
  low <- sts$Stratum[sts$nHs < 5]
  if (length(low)) flags <- c(flags, sprintf("%d of %d strata have fewer than 5 expected heterozygotes per locus (median n*Hs): within-stratum variance is poorly estimated", length(low), K))
  if (cm$no > 1) flags <- c(flags, sprintf("the graph has %d components: test within components", cm$no))
  if (isTRUE(gr[["degree_share"]] > 0.15)) flags <- c(flags, sprintf("degree share %.2f at gamma = %g: the source-sink score is partly set by node degree", gr[["degree_share"]], gamma))
  if (K < 10) flags <- c(flags, "fewer than 10 strata: neither directional test is likely to certify direction from one graph; report signs as descriptions")
  else if (K < 15) flags <- c(flags, "10-14 strata: the source-sink test has low power; prefer the directional isolation comparison with a cutoff from popgraph_null()")
  if (sum(poly) < 10) flags <- c(flags, sprintf("%d polymorphic loci: power of the source-sink test falls and the Delta R^2 > 0 rule becomes liberal; calibrate with popgraph_null()", sum(poly)))

  out <- list(design = c(strata = K, individuals = N, n_min = min(n_i), n_median = stats::median(n_i), loci = length(loci),
                         polymorphic = sum(poly), alleles_median = stats::median(A), gst = gs[["gst"]], hs = gs[["hs"]]),
              strata = sts, estimate = attr(graph, "design"), graph = gr, falconer = fal, flags = flags)
  class(out) <- "popgraph_diagnostics"
  out
}

#' @rdname popgraph_diagnostics
#' @param ... Ignored.
#' @export
print.popgraph_diagnostics <- function(x, ...) {
  d <- x$design; g <- x$graph; e <- x$estimate
  cat("Population Graph sampling diagnostics\n")
  cat(sprintf("  %d strata, %d individuals (per stratum %d-%g, median %g); %d loci (%d polymorphic; median %g alleles)\n",
              d[["strata"]], d[["individuals"]], d[["n_min"]], max(x$strata$n), d[["n_median"]], d[["loci"]], d[["polymorphic"]], d[["alleles_median"]]))
  cat(sprintf("  Gst %.3f; mean Hs %.3f; median n*Hs per stratum %.1f (min %.1f)\n", d[["gst"]], d[["hs"]],
              stats::median(x$strata$nHs), min(x$strata$nHs)))
  if (!is.null(e)) cat(sprintf("  estimate: %d rotated variables on %d within-stratum df (%d constant within strata); %d discriminant axes\n",
                               e$dims, e$df_within, e$constant_within, e$rank_between))
  cat(sprintf("  graph: %d edges, %d component(s) (largest %d), mean degree %.1f, %d leaves; degree share %.2f, cor(S, S0) %.2f\n",
              g[["edges"]], g[["components"]], g[["largest"]], g[["mean_degree"]], g[["leaves"]], g[["degree_share"]], g[["cor_S_S0"]]))
  cat(sprintf("  Falconer optimum for Phi at N = %d: %g strata of %g (optimal_sampling())\n",
              d[["individuals"]], x$falconer[["strata"]], x$falconer[["per_stratum"]]))
  if (length(x$flags)) { cat("\n"); for (f in x$flags) cat("  * ", f, "\n", sep = "") }
  cat("\n")
  invisible(x)
}
