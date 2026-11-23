#' Design-matched symmetric null for directional tests on a Population Graph
#'
#' The false-positive rate of the directional isolation comparison
#' (\code{\link{ibgd}} with \code{mode = "dcGD"}, decision rule
#' \eqn{\Delta R^2 > 0}) depends on the sampling design: it rises as the number
#' of strata falls and changes with the number of individuals per stratum and of
#' loci.  A cutoff calibrated on one design does not transfer to another.  This
#' function calibrates the comparison, and the source--sink gradient statistic,
#' for the design at hand by simulating data sets that share it but are produced
#' by symmetric migration, rebuilding the Population Graph for each, and
#' returning the null distributions of \eqn{\Delta R^2} and \eqn{r(S, x)}.
#'
#' Each null data set keeps the observed strata, the number of genotyped
#' individuals in each stratum, the loci with their alleles, and which
#' individuals are missing at which locus.  Each null graph is built with the
#' \code{alpha} and \code{tol} recorded in the \code{"design"} attribute of the
#' observed graph (\code{\link{popgraph}} defaults if it has none).  Every stratum
#' starts at the pooled observed allele frequencies and evolves for \code{ngen}
#' generations under a Wright--Fisher model on allele frequencies: migration
#' through a symmetric, doubly stochastic matrix \eqn{M}, then multinomial
#' sampling of \eqn{2N_e} gene copies.  The observed number of individuals is
#' then drawn from each stratum as Hardy--Weinberg genotypes and passed to
#' \code{\link{popgraph}}.  Unless \code{migration} is given,
#' \eqn{M = (1-m)I + mB}, where \eqn{B} is the symmetric, doubly stochastic
#' (Sinkhorn) balancing of the kernel \eqn{k_{ij} = \exp(-d_{ij}/s)}
#' (\eqn{k_{ii} = 0}) on the kernel distances among strata
#' (\code{kernel_distance}, by default \eqn{|x_i - x_j|}), with \eqn{s} the
#' largest nearest-neighbour distance.  Every stratum therefore sends and
#' receives a fraction \eqn{m} of its genes, weighted toward near strata, however
#' unevenly the strata are spaced.  The migration rate \eqn{m} is tuned by
#' bisection so that the mean differentiation of the simulated samples matches
#' that of the observed sample (\eqn{G_{ST}}, Nei & Chesser 1983, computed the
#' same way for both).  Matching differentiation matters: under strong drift a
#' cutoff calibrated at weaker differentiation is too liberal.  Because \eqn{M}
#' is symmetric there is no net flow in any direction, so every rejection in the
#' null is a false positive.
#'
#' The null is specific to its migration model (a symmetric distance kernel by
#' default) and to \code{Ne} and \code{ngen}, which together set how much
#' diversity is lost before sampling; the result reports the simulated and
#' observed within-stratum diversity so the match can be judged, and the
#' cutoff should be checked for sensitivity to \code{Ne} and \code{ngen}.
#'
#' \strong{Strata that differ in diversity.}  By default every stratum has the
#' same \code{Ne}, so the null strata differ in diversity only by chance.  Real
#' strata often do not: a gradient in within-stratum diversity, such as a decline
#' away from a refugium, is itself a common signature of range expansion.
#' Supplying one \code{Ne} per stratum lets the null reproduce such differences
#' through drift alone.  Migration stays symmetric, so the null still contains
#' no net flow; it asks whether diversity differences, without directional gene
#' flow, could produce the observed statistics.  The \code{strata} element of
#' the result compares the observed and simulated \eqn{H_S} of every stratum so
#' that the chosen \code{Ne} can be judged; \eqn{m} is tuned to the observed
#' \eqn{G_{ST}} as before.  Because migration is retuned and evens out the
#' diversity of neighbouring strata, matching every stratum exactly is rarely
#' possible; a smooth profile (e.g. \code{Ne} log-linear along \code{axis})
#' fitted to the observed trend is usually the better choice.
#'
#' \strong{Graph structure.}  Besides the two test statistics, each null graph
#' records how strongly degree shapes it: the degree share, \eqn{cor(S, S^0)}
#' (the correlation of the source--sink score with its degree-only value), the
#' variance of degree and the ratio of maximum to mean degree.  The
#' \code{structure} element gives the percentile of each observed value in its
#' null.  An observed graph far in the upper tail is more hub-like than
#' symmetric exchange produces for this design, so its source--sink scores
#' largely restate which strata are hubs.
#'
#' @param x A \code{data.frame} of genotypes with \code{locus} columns and a
#'   stratum column.
#' @param stratum Name of the stratum column (default \code{"Population"}, or the
#'   grouping of a grouped data frame).
#' @param axis A numeric vector named by stratum: position along the
#'   hypothesized axis of flow (as \code{x} in \code{\link{ibgd}} and
#'   \code{\link{source_sink_test}}).  A pair is forward when \eqn{x_j > x_i}.
#' @param distance,forward Optional stratum-by-stratum matrices passed to
#'   \code{\link{ibgd}} in place of \code{axis} (separation, and \code{TRUE} for
#'   forward pairs).  \code{axis} is still used for \eqn{r(S, x)}.
#' @param graph The observed Population Graph (default
#'   \code{population_graph(x, stratum)}).  Supply it when the test is run on a
#'   component of a larger graph; its nodes must be the strata of \code{x}.  A
#'   graph made with \code{population_graph(x, alpha = )} passes its edge-retention
#'   level to the null graphs.
#' @param nsim Number of null data sets (default 999).
#' @param gamma Bandwidth multiplier (default 0.5).
#' @param Ne Effective size of each stratum in the simulation (default 100):
#'   one value for every stratum, or one per stratum, named by stratum (any
#'   order) or unnamed in the order of the sorted stratum levels.  Values are
#'   rounded to whole individuals (at least 1).  See \emph{Strata that differ
#'   in diversity}.
#' @param ngen Generations simulated before sampling (default
#'   \code{4 * mean(Ne)}, rounded).
#' @param migration Optional symmetric stratum-by-stratum migration matrix
#'   (rows sum to one), used as is in place of the tuned kernel.
#' @param kernel_distance Optional stratum-by-stratum distances for the
#'   migration kernel (default \eqn{|x_i - x_j|}).
#' @param phi Optional target \eqn{G_{ST}} (default: that of the observed sample).
#' @param alpha Size of the cutoff (default 0.05: the 95th percentile).
#' @param pilot Replicates per step of the bisection on \eqn{m} (default 8).
#' @param cores Cores for the null replicates (\code{parallel::mclapply};
#'   default 1).
#' @param verbose Print progress (default \code{FALSE}).
#' @return An object of class \code{popgraph_null}:
#'   \describe{
#'     \item{\code{observed}}{The statistics of the observed graph, named as
#'       the columns of \code{null} up to \code{max_mean_degree}.}
#'     \item{\code{null}}{One row per null data set: \code{dR2},
#'       \code{R2_cGD}, \code{slope_ratio}, \code{r_S}, \code{degree_share},
#'       \code{n_comp}, \code{cor_S_S0}, \code{var_degree},
#'       \code{max_mean_degree}, \code{gst}, \code{hs}.}
#'     \item{\code{cutoff}}{The \eqn{1-\alpha} quantile of null
#'       \eqn{\Delta R^2}, floored at 0, over all null graphs (\code{all})
#'       and over connected ones (\code{connected}).}
#'     \item{\code{p.value}}{One-sided for \eqn{\Delta R^2} (\code{dR2}),
#'       two-sided for \eqn{r(S, x)} (\code{r_S}).}
#'     \item{\code{fpr_rule}}{The rate at which \eqn{\Delta R^2 > 0} in the
#'       null.}
#'     \item{\code{structure}}{The percentile (fraction of null graphs at or
#'       below the observed value) of the observed \code{degree_share},
#'       \code{cor_S_S0}, \code{var_degree} and \code{max_mean_degree}.}
#'     \item{\code{strata}}{One row per stratum: \code{Stratum}, \code{n}
#'       (genotyped individuals), \code{Ne}, \code{hs_observed} and
#'       \code{hs_null} (mean over the null data sets of the sampled
#'       \eqn{H_S}, unbiased and averaged over loci).}
#'     \item{\code{tuning}}{\eqn{m}, the kernel scale, the target and
#'       achieved \eqn{G_{ST}}, observed and simulated \eqn{H_S}, the mean
#'       \code{Ne} and \code{ngen}.}
#'     \item{\code{design}, \code{alpha}, \code{gamma}, \code{nsim}}{The
#'       design (strata, individuals per stratum, loci, alleles) and settings.}
#'   }
#' @seealso \code{\link{ibgd}}, \code{\link{source_sink_test}},
#'   \code{\link{popgraph_diagnostics}}, \code{\link{optimal_sampling}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' \donttest{
#' data(arapat)
#' d <- droplevels(arapat[arapat$Species == "Peninsula", ])
#' d <- d[d$Population %in% names(which(table(d$Population) >= 8)), ]
#' d <- droplevels(d)
#' lat <- tapply(d$Latitude, d$Population, mean)
#' nl <- popgraph_null(d, axis = -lat, nsim = 19, ngen = 100)
#' nl
#' nl$structure
#'
#' # Effective sizes that decline southward (a diversity gradient), mean 100;
#' # compare observed and simulated Hs per stratum
#' z <- as.vector(scale(lat))
#' ne <- setNames(100 * exp(0.5 * z) / mean(exp(0.5 * z)), names(lat))
#' nl2 <- popgraph_null(d, axis = -lat, Ne = ne, nsim = 19, ngen = 100)
#' nl2$strata
#' }
#' @export
popgraph_null <- function(x, stratum = "Population", axis, distance = NULL, forward = NULL,
                          graph = NULL, nsim = 999L, gamma = 0.5, Ne = 100L, ngen = 4L * mean(Ne),
                          migration = NULL, kernel_distance = NULL, phi = NULL, alpha = 0.05,
                          pilot = 8L, cores = 1L, verbose = FALSE) {
  if (!is.data.frame(x)) stop("'x' must be a data.frame")
  stratum <- .detect_stratum(x, stratum, default = "Population", explicit = !missing(stratum))
  x <- .plain_df(x)
  if (!(stratum %in% names(x))) stop(sprintf("cannot find the stratum column '%s'", stratum))
  loci <- column_class(x, "locus")
  if (!length(loci)) stop("'x' has no locus columns")
  grp <- factor(as.character(x[[stratum]])); strata <- levels(grp); K <- length(strata)
  if (K < 4) stop("at least four strata are needed")
  if (missing(axis) || is.null(names(axis)) || !all(strata %in% names(axis)))
    stop("'axis' must be a numeric vector named by every stratum")
  axis <- axis[strata]
  Ne <- .pgn_check_Ne(Ne, strata)
  ngen <- as.integer(round(ngen))
  if (length(ngen) != 1L || is.na(ngen) || ngen < 0L) stop("'ngen' must be a single non-negative number")
  if (is.null(graph)) graph <- population_graph(x, stratum = stratum)
  if (!setequal(igraph::V(graph)$name, strata)) stop("the nodes of 'graph' must be the strata of 'x'")

  # observed design: per-locus dosage matrices, individuals per stratum, pooled frequencies
  mv <- lapply(loci, function(l) { m <- to_mv(x[[l]]); m[rowSums(m) == 0, ] <- NA; m })
  names(mv) <- loci
  keep <- vapply(mv, function(m) ncol(m) > 1, TRUE)
  mv <- mv[keep]
  n_i <- c(table(grp))
  p0 <- lapply(mv, function(m) { p <- colMeans(m, na.rm = TRUE); p / sum(p) })
  fr_obs <- lapply(mv, function(m) .pgn_freqs(m, grp))
  obs_gst <- .pgn_gst(fr_obs, n_i)
  obs_hs_site <- .pgn_site_hs(fr_obs)
  # observed missing genotypes, with rows in the stratum order of the simulated data
  ord <- order(grp)
  miss <- lapply(mv, function(m) !stats::complete.cases(m)[ord])
  n_l <- lapply(mv, function(m) c(table(factor(grp[stats::complete.cases(m)], levels = strata))))
  # null graphs are built as the observed graph was
  gd <- attr(graph, "design")
  g_alpha <- if (is.null(gd$alpha)) 0.05 else gd$alpha
  g_tol <- if (is.null(gd$tol)) 1e-4 else gd$tol

  stat <- function(g) .pgn_stats(g, axis, distance, forward, gamma)
  observed <- stat(graph)

  # migration matrix: supplied, or a symmetric kernel tuned to the observed differentiation
  if (is.null(migration)) {
    D <- if (is.null(kernel_distance)) abs(outer(axis, axis, "-")) else as.matrix(kernel_distance)[strata, strata]
    dimnames(D) <- list(strata, strata)
    nn <- apply(D + diag(Inf, K), 1, min); s <- max(nn[is.finite(nn) & nn > 0])
    B <- .pgn_balance(exp(-D / s))
    target <- if (is.null(phi)) obs_gst[["gst"]] else phi
    # Gst is undefined (NA) for a sample in which every locus is fixed; such pilot
    # replicates are dropped, and an all-NA step counts as above target (more migration)
    sim_gst <- function(m) mean(replicate(pilot, .pgn_gst(.pgn_sample_freqs(.pgn_wf(p0, .pgn_mix(B, m), Ne, ngen), n_l), n_i)[["gst"]]),
                                na.rm = TRUE)
    lo <- log(1e-4); hi <- log(0.95)
    for (it in seq_len(12)) {
      mid <- (lo + hi) / 2; g_mid <- sim_gst(exp(mid))
      if (verbose) message(sprintf("tuning m = %.4g: Gst %.4f (target %.4f)", exp(mid), g_mid, target))
      if (is.na(g_mid) || g_mid > target) lo <- mid else hi <- mid
    }
    m_hat <- exp((lo + hi) / 2); M <- .pgn_mix(B, m_hat)
  } else {
    M <- as.matrix(migration)[strata, strata]
    if (max(abs(M - t(M))) > 1e-10) warning("'migration' is not symmetric: the null then contains net flow")
    m_hat <- NA_real_; target <- if (is.null(phi)) obs_gst[["gst"]] else phi; s <- NA_real_
  }

  one <- function(b) {
    P <- .pgn_wf(p0, M, Ne, ngen)
    X <- .pgn_genotypes(P, n_i, miss)
    fr <- lapply(X$by_locus, function(m) .pgn_freqs(m, X$groups)); gs <- .pgn_gst(fr, n_i)
    g <- tryCatch(suppressWarnings(popgraph(X$mv, groups = X$groups, alpha = g_alpha, tol = g_tol)),
                  error = function(e) NULL)
    st <- if (is.null(g)) rep(NA_real_, length(observed)) else stat(g)
    c(st, gst = gs[["gst"]], hs = gs[["hs"]], .pgn_site_hs(fr)[strata])
  }
  res <- if (cores > 1L) parallel::mclapply(seq_len(nsim), one, mc.cores = cores) else lapply(seq_len(nsim), one)
  res <- do.call(rbind, res)
  ns <- length(observed) + 2L
  null <- as.data.frame(res[, seq_len(ns), drop = FALSE])
  names(null) <- c(names(observed), "gst", "hs")
  hs_site_null <- colMeans(res[, ns + seq_len(K), drop = FALSE], na.rm = TRUE)

  if (is.finite(target) && abs(mean(null$gst, na.rm = TRUE) - target) > 0.2 * target)
    warning(sprintf("null Gst %.3f differs from the target %.3f by more than 20%%; adjust Ne, ngen or the kernel",
                    mean(null$gst, na.rm = TRUE), target))
  q <- function(z) max(0, unname(stats::quantile(z, 1 - alpha, na.rm = TRUE)))
  pct <- function(v) if (all(is.na(null[[v]])) || is.na(observed[[v]])) NA_real_ else
    mean(null[[v]] <= observed[[v]] + 1e-12, na.rm = TRUE)
  conn <- null$n_comp == 1
  out <- list(
    observed = observed,
    null     = null,
    cutoff   = c(all = q(null$dR2), connected = if (any(conn, na.rm = TRUE)) q(null$dR2[conn]) else NA_real_),
    p.value  = c(dR2 = (1 + sum(null$dR2 >= observed[["dR2"]], na.rm = TRUE)) / (1 + sum(!is.na(null$dR2))),
                 r_S = (1 + sum(abs(null$r_S) >= abs(observed[["r_S"]]) - 1e-12, na.rm = TRUE)) / (1 + sum(!is.na(null$r_S)))),
    fpr_rule = mean(null$dR2 > 0, na.rm = TRUE),
    structure = vapply(c(degree_share = "degree_share", cor_S_S0 = "cor_S_S0", var_degree = "var_degree",
                         max_mean_degree = "max_mean_degree"), pct, 1),
    strata   = data.frame(Stratum = strata, n = as.integer(n_i), Ne = unname(Ne),
                          hs_observed = unname(obs_hs_site[strata]), hs_null = unname(hs_site_null),
                          row.names = NULL, stringsAsFactors = FALSE),
    tuning   = c(m = m_hat, scale = s, gst_target = target, gst_null = mean(null$gst, na.rm = TRUE),
                 hs_observed = obs_gst[["hs"]], hs_null = mean(null$hs, na.rm = TRUE), Ne = mean(Ne), ngen = ngen),
    design   = list(K = K, n = n_i, loci = names(mv), alleles = vapply(p0, length, 1L)),
    alpha = alpha, gamma = gamma, nsim = nsim)
  class(out) <- "popgraph_null"
  out
}

#' @rdname popgraph_null
#' @param ... Ignored.
#' @export
print.popgraph_null <- function(x, ...) {
  o <- x$observed; tu <- x$tuning
  cat("Design-matched symmetric null for a Population Graph\n")
  cat(sprintf("  design: %d strata, %s individuals per stratum (median %g), %d loci (%s alleles)\n",
              x$design$K, paste(range(x$design$n), collapse = "-"), stats::median(x$design$n),
              length(x$design$loci), paste(range(x$design$alleles), collapse = "-")))
  ne <- if (is.null(x$strata)) tu[["Ne"]] else x$strata$Ne
  ne_txt <- if (length(unique(ne)) == 1L) sprintf("Ne = %g", ne[1]) else
    sprintf("Ne = %g-%g (mean %.4g)", min(ne), max(ne), mean(ne))
  cat(sprintf("  null: %d data sets; Wright-Fisher, %s, %g generations, m = %.4g; Gst %.3f (target %.3f); Hs %.3f (observed %.3f)\n",
              x$nsim, ne_txt, tu[["ngen"]], tu[["m"]], tu[["gst_null"]], tu[["gst_target"]], tu[["hs_null"]], tu[["hs_observed"]]))
  if (!is.null(x$strata) && length(unique(ne)) > 1L && sum(is.finite(x$strata$hs_null)) > 2L)
    cat(sprintf("  per-stratum Hs, observed vs null: r = %.2f\n",
                stats::cor(x$strata$hs_observed, x$strata$hs_null, use = "complete.obs")))
  cat(sprintf("  null graphs fragmented: %.0f%%; rule dR2 > 0 fires in %.1f%% of null graphs\n",
              100 * mean(x$null$n_comp > 1, na.rm = TRUE), 100 * x$fpr_rule))
  cat(sprintf("\n  Directional isolation: observed dR2 = %.4f; cutoff (%g%%) = %.4f [connected null graphs: %.4f]; one-sided p = %.4f\n",
              o[["dR2"]], 100 * (1 - x$alpha), x$cutoff[["all"]], x$cutoff[["connected"]], x$p.value[["dR2"]]))
  cat(sprintf("  Source-sink gradient: observed r(S, x) = %.3f; design-null two-sided p = %.4f\n",
              o[["r_S"]], x$p.value[["r_S"]]))
  if (!is.null(x$structure)) {
    cat(sprintf("  Graph structure (percentile in null): degree share %.2f (%.1f%%); cor(S, S0) %.2f (%.1f%%); var(degree) %.2f (%.1f%%)\n\n",
                o[["degree_share"]], 100 * x$structure[["degree_share"]], o[["cor_S_S0"]], 100 * x$structure[["cor_S_S0"]],
                o[["var_degree"]], 100 * x$structure[["var_degree"]]))
  } else cat("\n")
  invisible(x)
}

## ---- internals --------------------------------------------------------------------------------

# Statistics of one graph: Delta R^2 and slope ratio (ibgd dcGD), r(S, x), degree share, components
.pgn_stats <- function(g, axis, distance, forward, gamma) {
  nodes <- igraph::V(g)$name
  ib <- tryCatch(if (is.null(distance)) ibgd(g, axis[nodes], mode = "dcGD", gamma = gamma)
                 else ibgd(g, mode = "dcGD", distance = distance, forward = forward, gamma = gamma),
                 error = function(e) NULL)
  sc <- tryCatch(source_sink_scores(g, gamma), error = function(e) NULL)
  r <- if (is.null(sc)) NA_real_ else suppressWarnings(stats::cor(.merge_ties(sc$S), axis[sc$Stratum], method = "spearman"))
  k <- igraph::degree(g)
  c(dR2 = if (is.null(ib)) NA_real_ else unname(ib$statistic),
    R2_cGD = if (is.null(ib)) NA_real_ else unname(ib$estimate[["R2 cGD"]]),
    slope_ratio = if (is.null(ib)) NA_real_ else unname(ib$estimate[["slope ratio"]]),
    r_S = r, degree_share = if (is.null(sc)) NA_real_ else attr(sc, "degree_share"),
    n_comp = igraph::components(g)$no,
    cor_S_S0 = if (is.null(sc)) NA_real_ else attr(sc, "cor_S_S0"),
    var_degree = if (length(k) > 1L) stats::var(k) else NA_real_,
    max_mean_degree = if (length(k) && mean(k) > 0) max(k) / mean(k) else NA_real_)
}

# Ne: one value, or one per stratum (named by stratum, or unnamed in stratum order);
# returned named by stratum, rounded to whole individuals (2 Ne gene copies are binomial sizes)
.pgn_check_Ne <- function(Ne, strata) {
  K <- length(strata)
  if (!is.numeric(Ne) || !length(Ne) || anyNA(Ne) || any(!is.finite(Ne)) || any(Ne <= 0))
    stop("'Ne' must be positive and finite")
  if (length(Ne) == 1L) Ne <- rep(Ne, K)
  else if (!is.null(names(Ne))) {
    if (!all(strata %in% names(Ne))) stop("'Ne' must be named by every stratum (or have one value per stratum)")
    Ne <- Ne[strata]
  } else if (length(Ne) != K) stop(sprintf("'Ne' must have length 1 or %d (one per stratum)", K))
  stats::setNames(pmax(1, round(as.numeric(Ne))), strata)
}

# Per-stratum unbiased Hs averaged over loci, from per-locus stratum frequencies (.pgn_freqs)
.pgn_site_hs <- function(fr) {
  K <- nrow(fr[[1]]$p)
  he <- vapply(fr, function(f) (2 * f$n / pmax(2 * f$n - 1, 1)) * (1 - rowSums(f$p^2)), numeric(K))
  he <- matrix(he, nrow = K)
  stats::setNames(rowMeans(he, na.rm = TRUE), rownames(fr[[1]]$p))
}

# Stratum allele frequencies from a dosage matrix (rows individuals, NA rows missing)
.pgn_freqs <- function(m, grp) {
  ok <- stats::complete.cases(m); lv <- levels(grp)
  n <- c(table(factor(grp[ok], levels = lv)))
  p <- matrix(NA_real_, length(lv), ncol(m), dimnames = list(lv, colnames(m)))
  if (any(ok)) { s <- rowsum(m[ok, , drop = FALSE], as.character(grp[ok])); p[rownames(s), ] <- s / n[rownames(s)] }
  list(p = p, n = n)
}

# Multilocus Gst (Nei & Chesser 1983) and mean Hs from per-locus stratum frequencies
.pgn_gst <- function(fr, n_i) {
  hs <- ht <- numeric(length(fr))
  for (l in seq_along(fr)) {
    p <- fr[[l]]$p; n <- fr[[l]]$n; ok <- n > 1 & is.finite(rowSums(p)); p <- p[ok, , drop = FALSE]; n <- n[ok]
    K <- nrow(p); nh <- K / sum(1 / n)
    h_i <- (2 * n / (2 * n - 1)) * (1 - rowSums(p^2))
    hs[l] <- mean(h_i); ht[l] <- 1 - sum(colMeans(p)^2) + hs[l] / (2 * nh * K)
  }
  c(gst = (sum(ht) - sum(hs)) / sum(ht), hs = mean(hs))
}

# Symmetric Sinkhorn balancing of a symmetric kernel (zero diagonal): B = diag(d) k diag(d)
# with unit row and column sums
.pgn_balance <- function(k, tol = 1e-10, maxit = 10000L) {
  diag(k) <- 0; d <- rep(1, nrow(k))
  for (it in seq_len(maxit)) {
    d_new <- sqrt(d / as.vector(k %*% d))
    if (max(abs(d_new - d)) < tol) { d <- d_new; break }
    d <- d_new
  }
  B <- k * outer(d, d); B / rowSums(B)                       # final renormalization (rounding)
}
# Migration matrix with emigration fraction m through the balanced kernel B
.pgn_mix <- function(B, m) { M <- m * B; diag(M) <- 1 - m; M }

# Wright-Fisher on allele frequencies: per generation, migration (P <- M P) then multinomial
# drift of 2 Ne gene copies per stratum. p0: list of pooled allele frequencies per locus.
.pgn_wf <- function(p0, M, Ne, ngen) {
  K <- nrow(M); twoN <- rep_len(2 * Ne, K)
  lapply(p0, function(p) {
    P <- matrix(p, K, length(p), byrow = TRUE)
    for (g in seq_len(ngen)) P <- .pgn_drift(M %*% P, twoN)
    P
  })
}
.pgn_drift <- function(P, twoN) {
  A <- ncol(P); C <- matrix(0, nrow(P), A); left <- twoN; rest <- rep(1, nrow(P))
  for (a in seq_len(A - 1L)) {
    pr <- ifelse(rest > 1e-12, pmin(1, pmax(0, P[, a] / rest)), 0)
    C[, a] <- stats::rbinom(nrow(P), left, pr); left <- left - C[, a]; rest <- rest - P[, a]
  }
  C[, A] <- left
  C / twoN
}

# Sampled stratum frequencies (for tuning). n: per stratum genotyped individuals, one vector
# for all loci or a list with one vector per locus (strata with none give NA frequencies)
.pgn_sample_freqs <- function(P, n) {
  if (!is.list(n)) n <- rep(list(n), length(P))
  Map(function(Pl, nl) {
    f <- t(vapply(seq_len(nrow(Pl)), function(i)
      if (nl[[i]] > 0) tabulate(sample.int(ncol(Pl), 2L * nl[[i]], TRUE, Pl[i, ]), ncol(Pl)) / (2 * nl[[i]])
      else rep(NA_real_, ncol(Pl)), numeric(ncol(Pl))))
    list(p = f, n = nl)
  }, P, n)
}

# Hardy-Weinberg genotypes: dosage/2 matrices (as to_mv()) for n_i individuals per stratum,
# rows ordered by stratum.  miss: optional list (one logical vector per locus, in that row
# order) of genotypes to leave missing -- coded 0 in 'mv' (as to_mv()) and NA in 'by_locus'
.pgn_genotypes <- function(P, n_i, miss = NULL) {
  groups <- factor(rep(names(n_i), n_i), levels = names(n_i)); N <- sum(n_i)
  by_locus <- lapply(seq_along(P), function(l) {
    Pl <- P[[l]]; A <- ncol(Pl); m <- matrix(0, N, A); row0 <- 0L
    for (i in seq_len(nrow(Pl))) {
      ni <- n_i[[i]]; al <- matrix(sample.int(A, 2L * ni, TRUE, Pl[i, ]), ni, 2L); r <- row0 + seq_len(ni)
      m[cbind(r, al[, 1])] <- m[cbind(r, al[, 1])] + 0.5; m[cbind(r, al[, 2])] <- m[cbind(r, al[, 2])] + 0.5
      row0 <- row0 + ni
    }
    if (!is.null(miss)) m[miss[[l]], ] <- NA
    m
  })
  names(by_locus) <- names(P)
  mv <- do.call(cbind, by_locus); mv[is.na(mv)] <- 0
  list(mv = mv, by_locus = by_locus, groups = groups)
}
