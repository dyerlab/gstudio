#' Bootstrap confidence intervals for edge asymmetry
#'
#' @description
#' Estimates how strongly, and how precisely, gene flow between two connected
#' populations is skewed in one direction, giving each edge's asymmetry index
#' \eqn{\Delta_{ij}} a confidence interval that can be reported, compared
#' across edges, and carried into evolutionary or management interpretation.
#' Individuals are resampled with replacement within each stratum, the
#' Population Graph and its asymmetries are re-estimated, and a percentile
#' interval is formed for every observed edge.
#'
#' @details
#' This is an estimation procedure, not a hypothesis test, and is the natural
#' companion to \code{\link{asymmetry_significance}}: once an edge has survived
#' the hierarchy of permutation tests there (\code{"existence"} \eqn{\to}
#' \code{"location"} \eqn{\to} \code{"mechanism"}), \code{asymmetry_ci()}
#' supplies the interval to report alongside it, e.g.
#' \eqn{\Delta_{ij} = X} (95\% CI: \emph{low}--\emph{high}).
#'
#' A wide interval, or one that spans zero, indicates that the strength and
#' even the direction of the imbalance depend on which individuals happened to
#' be sampled, so the edge should not be interpreted quantitatively, however it
#' fared in the significance tests.  An interval that excludes zero should
#' \emph{not} itself be read as evidence of significance: \eqn{\Delta_{ij}} is
#' generally non-zero on any fixed topology (for example through differences in
#' node degree), so zero is not a meaningful null for this statistic.  Use
#' \code{asymmetry_significance(mode = "location")} for that question.
#'
#' Because the graph is re-estimated on each resample, an observed edge may be
#' absent from some resamples; those resamples are dropped from that edge's
#' interval, so the interval is conditional on the edge being present.  The
#' proportion of resamples retaining the edge is returned as
#' \code{edge_support} and should be reported with the interval; a low value
#' marks an edge whose very existence, not just its asymmetry, is unstable.
#' Resampled edges are matched to observed edges by unordered endpoint pair,
#' and \eqn{\Delta} is sign-corrected to the observed orientation.
#'
#' A resample for which the graph or its asymmetries cannot be estimated at
#' all (for example because the re-estimated graph contains an isolated node)
#' carries no information about any edge.  Such resamples are excluded from
#' every interval and from the \code{edge_support} denominator, counted in
#' \code{attr(x, "nboot_failed")}, and reported with a warning.
#'
#' @param graph An undirected weighted \code{popgraph}/\code{igraph} object with
#'   a numeric \code{weight} edge attribute (the observed graph, used to define
#'   the edge set and orientations).
#' @param data The multivariate genotype matrix originally passed to
#'   \code{\link{popgraph}} (via \code{\link{to_mv}}).
#' @param groups A factor of stratum membership, one entry per row of
#'   \code{data}.
#' @param nboot Number of within-stratum bootstrap resamples (default 999).
#' @param conf Confidence level for the per-edge interval (default 0.95).
#' @param pendants How to treat pendant (leaf) edges, whose direction is
#'   topologically forced; one of \code{"warn"} (default), \code{"keep"}, or
#'   \code{"drop"}.  See \code{\link{asymmetry_significance}}.
#' @param ... Additional arguments passed to \code{\link{popgraph}} (e.g.
#'   \code{alpha}).
#'
#' @return A \code{data.frame} with one row per retained edge and the columns:
#'   \describe{
#'     \item{from, to}{Endpoint names of the edge.}
#'     \item{delta}{The observed asymmetry index \eqn{\Delta_{ij}}.}
#'     \item{boot_mean}{Mean of \eqn{\Delta_{ij}} across the resamples in
#'       which the edge was present.}
#'     \item{conf.low, conf.high}{Percentile bootstrap bounds at level
#'       \code{conf}.}
#'     \item{edge_support}{Proportion of successful resamples in which the
#'       edge was present (the denominator of the interval).}
#'   }
#'   The confidence level, number of resamples requested, and number of
#'   resamples that failed are stored in \code{attr(x, "conf")},
#'   \code{attr(x, "nboot")}, and \code{attr(x, "nboot_failed")}.
#'
#' @seealso \code{\link{asymmetry_significance}} for the permutation tests;
#'   \code{\link{graph_asymmetries}} for the asymmetry computation.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#'
#' @examples
#' \donttest{
#' data(arapat)
#' mv     <- to_mv(arapat)
#' groups <- arapat$Population
#' graph  <- popgraph(mv, groups)
#'
#' # A small nboot keeps the example quick; use 999 or more in practice.
#' ci <- asymmetry_ci(graph, data = mv, groups = groups, nboot = 9)
#' head(ci)
#' }
#'
#' @importFrom igraph as_edgelist E
#' @importFrom stats quantile
#' @export
asymmetry_ci <- function(graph, data, groups, nboot = 999, conf = 0.95,
                         pendants = c("warn", "keep", "drop"), ...) {

  pendants <- match.arg(pendants)
  .validate_asymmetry_graph(graph)
  .validate_asymmetry_data(data, groups)
  if (!is.numeric(conf) || length(conf) != 1L || conf <= 0 || conf >= 1)
    stop("'conf' must be a single number strictly between 0 and 1.")

  .validate_asymmetry_groups(graph, groups)
  groups    <- factor(as.character(groups))
  g_obs     <- graph_asymmetries(graph)
  el        <- igraph::as_edgelist(g_obs, names = TRUE)
  delta_obs <- igraph::E(g_obs)$delta
  ne        <- nrow(el)

  # unordered endpoint key for matching resampled edges to observed edges
  obs_key <- paste(pmin(el[, 1], el[, 2]), pmax(el[, 1], el[, 2]), sep = "\r")

  strata_idx <- split(seq_along(groups), groups)
  mat <- matrix(NA_real_, nrow = ne, ncol = nboot)
  ok  <- logical(nboot)

  for (b in seq_len(nboot)) {
    idx <- unlist(lapply(strata_idx,
                         function(ii) ii[sample.int(length(ii), replace = TRUE)]),
                  use.names = FALSE)

    gb <- tryCatch(
      graph_asymmetries(suppressWarnings(popgraph(data[idx, , drop = FALSE],
                                                  groups[idx], ...))),
      error = function(e) NULL
    )
    if (is.null(gb)) next
    ok[b] <- TRUE

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

    mat[, b] <- val
  }

  n_ok <- sum(ok)
  if (n_ok == 0)
    stop("None of the ", nboot, " bootstrap resamples produced a valid graph.")
  if (n_ok < nboot)
    warning(sprintf(paste0(
      "%d of %d bootstrap resamples failed to produce a valid graph and were ",
      "excluded; edge_support and the intervals are based on the remaining %d."),
      nboot - n_ok, nboot, n_ok))

  n_present <- rowSums(!is.na(mat))
  a         <- (1 - conf) / 2
  edge_q    <- function(x, p) {
    if (all(is.na(x))) NA_real_
    else stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE)
  }

  ret <- data.frame(
    from         = el[, 1],
    to           = el[, 2],
    delta        = delta_obs,
    boot_mean    = ifelse(n_present > 0, rowMeans(mat, na.rm = TRUE), NA_real_),
    conf.low     = apply(mat, 1, edge_q, p = a),
    conf.high    = apply(mat, 1, edge_q, p = 1 - a),
    edge_support = n_present / n_ok,
    stringsAsFactors = FALSE
  )

  ret <- .handle_pendant_edges(ret, graph, pendants)
  attr(ret, "conf")  <- conf
  attr(ret, "nboot") <- nboot
  attr(ret, "nboot_failed") <- nboot - n_ok
  return(ret)
}
