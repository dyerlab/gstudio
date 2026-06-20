#' Stability (jackknife/bootstrap) test for edge asymmetry
#'
#' @description
#' Evaluates whether the direction of each edge's asymmetry index
#' \eqn{\Delta_{ij}} is robust to the particular individuals sampled.
#' Individuals are resampled with replacement within each stratum, the
#' Population Graph and its asymmetries are re-estimated, and a resampling
#' confidence interval is formed for every observed edge.  An edge whose
#' interval excludes zero is directionally stable.
#'
#' @details
#' Unlike \code{\link{asymmetry_permutation}}, which asks whether an asymmetry
#' exceeds the graph's fixed geometry, this procedure asks whether a few outlier
#' individuals could be driving the signal.  Because the graph is re-estimated
#' on each resample, an observed edge may be absent in some resamples; such
#' resamples contribute \code{NA} for that edge and are dropped from its
#' interval.  Resampled edges are matched to the observed edges by unordered
#' endpoint pair, and \eqn{\Delta} is sign-corrected to the observed
#' orientation.  Usually called through \code{\link{asymmetry_significance}}
#' with \code{mode = "jackknife"}.
#'
#' @param graph An undirected weighted \code{popgraph}/\code{igraph} object
#'   (the observed graph, used to define the edge set and orientations).
#' @param data The multivariate genotype matrix passed to \code{\link{popgraph}}.
#' @param groups A factor of stratum membership, one entry per row of \code{data}.
#' @param nperm Number of within-stratum bootstrap resamples (default 999).
#' @param conf Confidence level for the per-edge interval (default 0.95).
#' @param ... Additional arguments passed to \code{\link{popgraph}}.
#'
#' @return A \code{data.frame} with columns \code{from}, \code{to},
#'   \code{delta} (observed), \code{statistic} (mean resampled \eqn{\Delta}),
#'   \code{p_value} (\code{NA} for this mode), \code{ci_low}, \code{ci_high}.
#'
#' @seealso \code{\link{asymmetry_significance}}, \code{\link{graph_asymmetries}}
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#'
#' @importFrom igraph as_edgelist E
#' @importFrom stats quantile
#' @export
asymmetry_jackknife <- function(graph, data, groups, nperm = 999,
                                conf = 0.95, ...) {

  groups    <- factor(as.character(groups))
  g_obs     <- graph_asymmetries(graph)
  el        <- igraph::as_edgelist(g_obs, names = TRUE)
  delta_obs <- igraph::E(g_obs)$delta
  ne        <- nrow(el)

  # unordered endpoint key for matching resampled edges to observed edges
  obs_key <- paste(pmin(el[, 1], el[, 2]), pmax(el[, 1], el[, 2]), sep = "\r")

  strata_idx <- split(seq_along(groups), groups)
  mat <- matrix(NA_real_, nrow = ne, ncol = nperm)

  for (p in seq_len(nperm)) {
    idx <- unlist(lapply(strata_idx,
                         function(ii) sample(ii, length(ii), replace = TRUE)),
                  use.names = FALSE)

    gb <- tryCatch(
      graph_asymmetries(suppressWarnings(popgraph(data[idx, , drop = FALSE],
                                                  groups[idx], ...))),
      error = function(e) NULL
    )
    if (is.null(gb)) next

    elb <- igraph::as_edgelist(gb, names = TRUE)
    if (nrow(elb) == 0) next
    db  <- igraph::E(gb)$delta
    key <- paste(pmin(elb[, 1], elb[, 2]), pmax(elb[, 1], elb[, 2]), sep = "\r")

    m   <- match(obs_key, key)
    val <- db[m]

    # If the resampled edge is listed in the reverse orientation, flip the sign
    # so it aligns with the observed (from -> to) direction.
    present <- !is.na(m)
    flip    <- present & (elb[m, 1] != el[, 1])
    val[flip] <- -val[flip]

    mat[, p] <- val
  }

  a       <- (1 - conf) / 2
  ci_low  <- apply(mat, 1, stats::quantile, probs = a,     na.rm = TRUE, names = FALSE)
  ci_high <- apply(mat, 1, stats::quantile, probs = 1 - a, na.rm = TRUE, names = FALSE)
  stat    <- rowMeans(mat, na.rm = TRUE)

  data.frame(
    from      = el[, 1],
    to        = el[, 2],
    delta     = delta_obs,
    statistic = stat,
    p_value   = NA_real_,
    ci_low    = ci_low,
    ci_high   = ci_high,
    stringsAsFactors = FALSE
  )
}
