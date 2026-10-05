# Internal: pendant (leaf) edge handling for per-edge results.
# Edges incident to a degree-one node carry a topologically forced direction
# (the leaf's only outgoing weight is 1), inflating |Delta| and biasing
# permutation p-values toward significance. Warn about or drop them on request.
#' @importFrom igraph degree
#' @keywords internal
#' @noRd
.handle_pendant_edges <- function(ret, graph, pendants) {
  if (pendants == "keep" || is.null(ret$from) || all(is.na(ret$from)))
    return(ret)

  deg     <- igraph::degree(graph)
  leaves  <- names(deg)[deg <= 1L]
  is_pend <- ret$from %in% leaves | ret$to %in% leaves
  n_pend  <- sum(is_pend, na.rm = TRUE)
  if (n_pend == 0L)
    return(ret)

  if (pendants == "warn") {
    warning(sprintf(paste0(
      "%d of %d edges are incident to a degree-one (pendant/leaf) node. ",
      "The direction on a pendant edge is topologically forced, so its ",
      "asymmetry magnitude is inflated and its p-value is anti-conservatively ",
      "biased toward significance. Interpret these edges with caution, or call ",
      "with pendants = 'drop' to exclude them."),
      n_pend, nrow(ret)))
  } else {                                        # pendants == "drop"
    nd  <- attr(ret, "null_distribution")
    ret <- ret[!is_pend, , drop = FALSE]
    rownames(ret) <- NULL
    if (!is.null(nd)) attr(ret, "null_distribution") <- nd[!is_pend, , drop = FALSE]
    attr(ret, "pendants_dropped") <- n_pend
  }
  ret
}
