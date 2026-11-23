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
#' The thresholds used for the checks come from simulations of a one-dimensional
#' stepping stone with 20 biallelic loci; they are guides, not tests.  For a
#' calibrated cutoff for a particular design use \code{\link{popgraph_null}}.
#'
#' @param x A \code{data.frame} of genotypes with \code{locus} columns and a
#'   stratum column.
#' @param stratum Name of the stratum column (default \code{"Population"}, or the
#'   grouping of a grouped data frame).
#' @param graph Optional Population Graph for \code{x} (default
#'   \code{population_graph(x, stratum)}).
#'   Its nodes must be the strata of \code{x}; to diagnose one component, subset
#'   \code{x} to its strata.
#' @param gamma Bandwidth multiplier for the degree-share check (default 0.5).
#' @param min_nHs Strata whose median expected number of heterozygotes per
#'   locus, \eqn{n_i H_{S,il}}, falls below this are counted as thinly sampled
#'   (default 5, from simulations with biallelic loci).
#' @return An object of class \code{popgraph_diagnostics}: \code{design}
#'   (strata, individuals per stratum, loci, alleles, polymorphic loci,
#'   \eqn{G_{ST}}), \code{strata} (per stratum: \eqn{n_i}, mean \eqn{H_S}, and the
#'   median over loci of \eqn{n_i H_{S,il}}), \code{estimate} (the
#'   \code{"design"} attribute of the graph), \code{graph} (components,
#'   largest component, mean degree, leaves, degree share, \eqn{cor(S, S^0)}),
#'   \code{falconer} (the \code{optimal_sampling()} optimum for the total
#'   sample size and \eqn{G_{ST}}) and \code{checks}: a \code{data.frame}
#'   with one row per check (\code{section}, \code{check}, \code{value},
#'   \code{threshold}, \code{status} -- \code{"ok"}, \code{"warn"}, or
#'   \code{NA} for a value reported without a threshold -- and \code{note}).
#'   \code{as.data.frame()} returns \code{checks}; \code{print()} shows it
#'   grouped by section, marking passed checks with a tick and failed
#'   ones with \code{"!"}.
#' @seealso \code{\link{popgraph_null}}, \code{\link{optimal_sampling}},
#'   \code{\link{source_sink_scores}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' data(arapat)
#' dg <- popgraph_diagnostics(arapat)
#' dg
#' as.data.frame(dg)
#' @export
popgraph_diagnostics <- function(x, stratum = "Population", graph = NULL, gamma = 0.5, min_nHs = 5) {
  if (!is.data.frame(x)) stop("'x' must be a data.frame")
  stratum <- .detect_stratum(x, stratum, default = "Population", explicit = !missing(stratum))
  x <- .plain_df(x)
  if (!(stratum %in% names(x))) stop(sprintf("cannot find the stratum column '%s'", stratum))
  loci <- column_class(x, "locus")
  grp <- factor(as.character(x[[stratum]])); strata <- levels(grp); K <- length(strata)
  if (is.null(graph)) graph <- population_graph(x, stratum = stratum)
  if (!setequal(igraph::V(graph)$name, strata))
    stop("the nodes of 'graph' must be the strata of 'x' (subset 'x' to diagnose a component)")

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

  Kc <- max(cm$csize)
  nlow <- sum(sts$nHs < min_nHs, na.rm = TRUE)
  ds <- gr[["degree_share"]]
  npoly <- sum(poly)
  e <- attr(graph, "design")

  # one row per check: status "ok" / "warn" when there is a threshold, NA when informational
  row <- function(section, check, value, threshold = NA_character_, status = NA_character_, note = NA_character_)
    data.frame(section = section, check = check, value = value, threshold = threshold,
               status = status, note = note, stringsAsFactors = FALSE)
  st <- function(bad) if (is.na(bad)) NA_character_ else if (bad) "warn" else "ok"
  checks <- rbind(
    row("Design", "strata in largest component",
        if (cm$no > 1) sprintf("%d of %d", Kc, K) else sprintf("%d", Kc), ">= 15", st(Kc < 15),
        if (Kc < 10) "too few to certify direction; report signs as descriptions"
        else if (Kc < 15) "source-sink test has low power; calibrate dR2 with popgraph_null()"
        else NA_character_),
    row("Design", "individuals per stratum",
        sprintf("%d-%d (median %g)", min(n_i), max(n_i), stats::median(n_i))),
    row("Design", "polymorphic loci", sprintf("%d of %d", npoly, length(loci)), ">= 10", st(npoly < 10),
        if (npoly < 10) "dR2 > 0 rule is liberal; calibrate with popgraph_null()" else NA_character_),
    row("Design", "alleles per locus", sprintf("median %g", stats::median(A))),
    row("Design", "Gst / mean Hs", sprintf("%.3f / %.3f", gs[["gst"]], gs[["hs"]])),
    row("Design", "expected hets (n*Hs)",
        sprintf("median %.1f (min %.1f)", stats::median(sts$nHs, na.rm = TRUE), min(sts$nHs, na.rm = TRUE)),
        sprintf(">= %g", min_nHs), st(nlow > 0),
        if (nlow > 0) sprintf("%d of %d strata below; within-stratum variance poorly estimated", nlow, K) else NA_character_),
    if (!is.null(e)) row("Estimate", "rotated variables / df", sprintf("%d on %d df", e$dims, e$df_within),
                         note = sprintf("%d dropped; %d discriminant axes", e$constant_within, e$rank_between)),
    row("Graph", "components", sprintf("%d (largest %d)", cm$no, Kc), "1", st(cm$no > 1),
        if (cm$no > 1) "test within a component" else NA_character_),
    row("Graph", "mean degree / leaves", sprintf("%.1f / %d", mean(k), sum(k == 1))),
    row("Graph", sprintf("degree share (gamma = %g)", gamma), sprintf("%.2f", ds), "<= 0.15", st(ds > 0.15),
        if (isTRUE(ds > 0.15)) "source-sink score partly set by node degree" else NA_character_),
    row("Graph", "cor(S, S0)", sprintf("%.2f", gr[["cor_S_S0"]])),
    row("Reference", "Falconer optimum for Phi",
        sprintf("%g strata of %g", fal[["strata"]], fal[["per_stratum"]]),
        note = "optimal_sampling(); generally too thin for a graph")
  )
  rownames(checks) <- NULL

  out <- list(design = c(strata = K, individuals = N, n_min = min(n_i), n_median = stats::median(n_i), loci = length(loci),
                         polymorphic = npoly, alleles_median = stats::median(A), gst = gs[["gst"]], hs = gs[["hs"]]),
              strata = sts, estimate = e, graph = gr, falconer = fal, checks = checks)
  class(out) <- "popgraph_diagnostics"
  out
}

#' @rdname popgraph_diagnostics
#' @param ... Ignored.
#' @export
print.popgraph_diagnostics <- function(x, ...) {
  d <- x$design; ck <- x$checks
  cat(sprintf("Population Graph sampling diagnostics: %d strata, %d individuals, %d loci\n",
              d[["strata"]], d[["individuals"]], d[["loci"]]))
  w1 <- max(nchar(ck$check)); w2 <- max(nchar(ck$value))
  thr <- ifelse(is.na(ck$threshold), "", paste0("(", ck$threshold, ")")); w3 <- max(nchar(thr))
  tick <- if (isTRUE(l10n_info()[["UTF-8"]])) "\u2713" else "+"
  mark <- ifelse(is.na(ck$status), " ", ifelse(ck$status == "warn", "!", tick))
  for (sec in unique(ck$section)) {
    cat("\n", sec, "\n", sep = "")
    for (i in which(ck$section == sec)) {
      line <- sprintf("  %-*s  %-*s  %-*s  %s", w1, ck$check[i], w2, ck$value[i], w3, thr[i], mark[i])
      if (!is.na(ck$note[i])) line <- paste0(line, "  ", ck$note[i])
      cat(sub("\\s+$", "", line), "\n", sep = "")
    }
  }
  nw <- sum(ck$status == "warn", na.rm = TRUE)
  cat(if (nw) sprintf("\n! %d check%s need%s attention (see ?popgraph_diagnostics)\n\n", nw,
                      if (nw > 1) "s" else "", if (nw > 1) "" else "s")
      else "\nAll checks passed.\n\n")
  invisible(x)
}

#' @rdname popgraph_diagnostics
#' @param row.names,optional Ignored.
#' @export
as.data.frame.popgraph_diagnostics <- function(x, row.names = NULL, optional = FALSE, ...) x$checks
