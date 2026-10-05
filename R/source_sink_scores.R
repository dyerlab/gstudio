#' @rdname genetic_gravity
#' @export
source_sink_scores <- function(graph, gamma = 0.5, x = NULL) {
  graph <- .gravity_check(graph)
  nodes <- igraph::V(graph)$name; K <- length(nodes)
  ed <- gravity_edges(graph, gamma, x)
  B <- matrix(0, nrow(ed), K, dimnames = list(NULL, nodes))
  B[cbind(seq_len(nrow(ed)), match(ed$from, nodes))] <- 1
  B[cbind(seq_len(nrow(ed)), match(ed$to, nodes))] <- -1
  bal <- as.numeric(crossprod(B, ed$Delta))                 # sum_j Delta_{i->j} = g_i - 1
  grav <- as.numeric(tapply(c(ed$w_from, ed$w_to), factor(c(ed$from, ed$to), nodes), sum))
  grav[is.na(grav)] <- 0
  L <- crossprod(B); k <- diag(L)
  comp <- igraph::components(graph)$membership[nodes]
  S <- numeric(K)
  for (cc in unique(comp)) {
    m <- comp == cc
    if (sum(m) > 1) { v <- as.numeric(MASS::ginv(L[m, m, drop = FALSE]) %*% bal[m]); S[m] <- v - mean(v) }
  }
  S0 <- ifelse(k > 0, -1 / k, NA_real_)
  S0 <- S0 - stats::ave(S0, comp, FUN = function(v) mean(v, na.rm = TRUE))
  fit <- as.numeric(B %*% S)
  ok <- k > 0
  sdz <- function(v) length(v) > 1 && stats::sd(v, na.rm = TRUE) > 0
  out <- data.frame(Stratum = nodes, S = S, gravity = grav, balance = bal, degree = as.numeric(k),
                    component = as.integer(comp), S0 = S0, n = NA_real_, stringsAsFactors = FALSE)
  if (!is.null(x)) {
    out$n <- as.numeric(colSums(B))
    attr(out, "dbar_decomposition") <- c(sum_delta = sum(ed$Delta), reproduced = sum(S * out$n),
                                         residual = sum(ed$Delta) - sum(S * out$n))
  }
  attr(out, "degree_share") <- if (sdz(ed$Delta) && sdz(ed$Delta0)) stats::cor(ed$Delta, ed$Delta0)^2 else NA_real_
  attr(out, "cor_S_S0") <- if (sdz(S[ok]) && sdz(S0[ok])) stats::cor(S[ok], S0[ok]) else NA_real_
  attr(out, "cor_S_degree") <- if (sdz(S[ok]) && sdz(k[ok])) stats::cor(S[ok], k[ok]) else NA_real_
  attr(out, "gradient_share") <- if (sum(ed$Delta^2) > 0) sum(fit^2) / sum(ed$Delta^2) else NA_real_
  attr(out, "gamma") <- gamma
  out
}
