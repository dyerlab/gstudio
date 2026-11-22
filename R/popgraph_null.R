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
#' individuals in each stratum, and the loci with their alleles.  Every stratum
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
#'   component of a larger graph; its nodes must be the strata of \code{x}.
#' @param nsim Number of null data sets (default 199).
#' @param gamma Bandwidth multiplier (default 0.5).
#' @param Ne Effective size of each stratum in the simulation (default 100).
#' @param ngen Generations simulated before sampling (default \code{4 * Ne}).
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
#' @return An object of class \code{popgraph_null}: \code{observed} (the
#'   statistics of the observed graph), \code{null} (one row per null data set:
#'   \code{dR2}, \code{slope_ratio}, \code{r_S}, \code{degree_share},
#'   \code{n_comp}, \code{gst}, \code{hs}), \code{cutoff} (the \eqn{1-\alpha}
#'   quantile of null \eqn{\Delta R^2}, floored at 0, over all null graphs and
#'   over connected ones), \code{p.value} (one-sided for \eqn{\Delta R^2},
#'   two-sided for \eqn{r(S, x)}), \code{fpr_rule} (the rate at which
#'   \eqn{\Delta R^2 > 0} in the null), and \code{tuning} (\eqn{m}, the target
#'   and achieved \eqn{G_{ST}}, observed and simulated \eqn{H_S}, \code{Ne},
#'   \code{ngen}).
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
#' }
#' @export
popgraph_null <- function(x, stratum = "Population", axis, distance = NULL, forward = NULL,
                          graph = NULL, nsim = 199L, gamma = 0.5, Ne = 100L, ngen = 4L * Ne,
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
  if (is.null(graph)) graph <- population_graph(x, stratum = stratum)
  if (!setequal(igraph::V(graph)$name, strata)) stop("the nodes of 'graph' must be the strata of 'x'")

  # observed design: per-locus dosage matrices, individuals per stratum, pooled frequencies
  mv <- lapply(loci, function(l) { m <- to_mv(x[[l]]); m[rowSums(m) == 0, ] <- NA; m })
  names(mv) <- loci
  keep <- vapply(mv, function(m) ncol(m) > 1, TRUE)
  mv <- mv[keep]
  n_i <- c(table(grp))
  p0 <- lapply(mv, function(m) { p <- colMeans(m, na.rm = TRUE); p / sum(p) })
  obs_gst <- .pgn_gst(lapply(mv, function(m) .pgn_freqs(m, grp)), n_i)

  stat <- function(g) .pgn_stats(g, axis, distance, forward, gamma)
  observed <- stat(graph)

  # migration matrix: supplied, or a symmetric kernel tuned to the observed differentiation
  if (is.null(migration)) {
    D <- if (is.null(kernel_distance)) abs(outer(axis, axis, "-")) else as.matrix(kernel_distance)[strata, strata]
    dimnames(D) <- list(strata, strata)
    nn <- apply(D + diag(Inf, K), 1, min); s <- max(nn[is.finite(nn) & nn > 0])
    B <- .pgn_balance(exp(-D / s))
    target <- if (is.null(phi)) obs_gst[["gst"]] else phi
    sim_gst <- function(m) mean(replicate(pilot, .pgn_gst(.pgn_sample_freqs(.pgn_wf(p0, .pgn_mix(B, m), Ne, ngen), n_i), n_i)[["gst"]]))
    lo <- log(1e-4); hi <- log(0.95)
    for (it in seq_len(12)) {
      mid <- (lo + hi) / 2; g_mid <- sim_gst(exp(mid))
      if (verbose) message(sprintf("tuning m = %.4g: Gst %.4f (target %.4f)", exp(mid), g_mid, target))
      if (g_mid > target) lo <- mid else hi <- mid
    }
    m_hat <- exp((lo + hi) / 2); M <- .pgn_mix(B, m_hat)
  } else {
    M <- as.matrix(migration)[strata, strata]
    if (max(abs(M - t(M))) > 1e-10) warning("'migration' is not symmetric: the null then contains net flow")
    m_hat <- NA_real_; target <- if (is.null(phi)) obs_gst[["gst"]] else phi; s <- NA_real_
  }

  one <- function(b) {
    P <- .pgn_wf(p0, M, Ne, ngen)
    X <- .pgn_genotypes(P, n_i)
    fr <- lapply(X$by_locus, function(m) .pgn_freqs(m, X$groups)); gs <- .pgn_gst(fr, n_i)
    g <- tryCatch(suppressWarnings(popgraph(X$mv, groups = X$groups)), error = function(e) NULL)
    st <- if (is.null(g)) rep(NA_real_, length(observed)) else stat(g)
    c(st, gst = gs[["gst"]], hs = gs[["hs"]])
  }
  res <- if (cores > 1L) parallel::mclapply(seq_len(nsim), one, mc.cores = cores) else lapply(seq_len(nsim), one)
  null <- as.data.frame(do.call(rbind, res))
  names(null)[seq_along(observed)] <- names(observed)

  if (is.finite(target) && abs(mean(null$gst, na.rm = TRUE) - target) > 0.2 * target)
    warning(sprintf("null Gst %.3f differs from the target %.3f by more than 20%%; adjust Ne, ngen or the kernel",
                    mean(null$gst, na.rm = TRUE), target))
  q <- function(z) max(0, unname(stats::quantile(z, 1 - alpha, na.rm = TRUE)))
  conn <- null$n_comp == 1
  out <- list(
    observed = observed,
    null     = null,
    cutoff   = c(all = q(null$dR2), connected = if (any(conn, na.rm = TRUE)) q(null$dR2[conn]) else NA_real_),
    p.value  = c(dR2 = (1 + sum(null$dR2 >= observed[["dR2"]], na.rm = TRUE)) / (1 + sum(!is.na(null$dR2))),
                 r_S = (1 + sum(abs(null$r_S) >= abs(observed[["r_S"]]) - 1e-12, na.rm = TRUE)) / (1 + sum(!is.na(null$r_S)))),
    fpr_rule = mean(null$dR2 > 0, na.rm = TRUE),
    tuning   = c(m = m_hat, scale = s, gst_target = target, gst_null = mean(null$gst, na.rm = TRUE),
                 hs_observed = obs_gst[["hs"]], hs_null = mean(null$hs, na.rm = TRUE), Ne = Ne, ngen = ngen),
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
  cat(sprintf("  null: %d data sets; Wright-Fisher, Ne = %g, %g generations, m = %.4g; Gst %.3f (target %.3f); Hs %.3f (observed %.3f)\n",
              x$nsim, tu[["Ne"]], tu[["ngen"]], tu[["m"]], tu[["gst_null"]], tu[["gst_target"]], tu[["hs_null"]], tu[["hs_observed"]]))
  cat(sprintf("  null graphs fragmented: %.0f%%; rule dR2 > 0 fires in %.1f%% of null graphs\n",
              100 * mean(x$null$n_comp > 1, na.rm = TRUE), 100 * x$fpr_rule))
  cat(sprintf("\n  Directional isolation: observed dR2 = %.4f; cutoff (%g%%) = %.4f [connected null graphs: %.4f]; one-sided p = %.4f\n",
              o[["dR2"]], 100 * (1 - x$alpha), x$cutoff[["all"]], x$cutoff[["connected"]], x$p.value[["dR2"]]))
  cat(sprintf("  Source-sink gradient: observed r(S, x) = %.3f; design-null two-sided p = %.4f (degree share %.2f)\n\n",
              o[["r_S"]], x$p.value[["r_S"]], o[["degree_share"]]))
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
  c(dR2 = if (is.null(ib)) NA_real_ else unname(ib$statistic),
    R2_cGD = if (is.null(ib)) NA_real_ else unname(ib$estimate[["R2 cGD"]]),
    slope_ratio = if (is.null(ib)) NA_real_ else unname(ib$estimate[["slope ratio"]]),
    r_S = r, degree_share = if (is.null(sc)) NA_real_ else attr(sc, "degree_share"),
    n_comp = igraph::components(g)$no)
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

# Sampled stratum frequencies (for tuning): n_i individuals from each stratum
.pgn_sample_freqs <- function(P, n_i) lapply(P, function(Pl) {
  f <- t(vapply(seq_len(nrow(Pl)), function(i) tabulate(sample.int(ncol(Pl), 2L * n_i[i], TRUE, Pl[i, ]), ncol(Pl)) / (2 * n_i[i]),
                numeric(ncol(Pl))))
  list(p = f, n = n_i)
})

# Hardy-Weinberg genotypes: dosage/2 matrices (as to_mv()) for n_i individuals per stratum
.pgn_genotypes <- function(P, n_i) {
  groups <- factor(rep(names(n_i), n_i), levels = names(n_i)); N <- sum(n_i)
  by_locus <- lapply(P, function(Pl) {
    A <- ncol(Pl); m <- matrix(0, N, A); row0 <- 0L
    for (i in seq_len(nrow(Pl))) {
      ni <- n_i[[i]]; al <- matrix(sample.int(A, 2L * ni, TRUE, Pl[i, ]), ni, 2L); r <- row0 + seq_len(ni)
      m[cbind(r, al[, 1])] <- m[cbind(r, al[, 1])] + 0.5; m[cbind(r, al[, 2])] <- m[cbind(r, al[, 2])] + 0.5
      row0 <- row0 + ni
    }
    m
  })
  list(mv = do.call(cbind, by_locus), by_locus = by_locus, groups = groups)
}
