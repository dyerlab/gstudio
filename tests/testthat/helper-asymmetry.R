# Shared fixtures for the asymmetry inference tests.

# Five simulated strata of 30 individuals each, returned with the popgraph
# built from them.  Skips if the simulation helpers are unavailable.
simulate_small_graph <- function() {
  skip_if_not(exists("make_population"), "make_population() unavailable")
  set.seed(1)
  freqs <- data.frame(
    Locus     = rep(c("L1", "L2", "L3"), each = 2),
    Allele    = rep(c("A", "B"), times = 3),
    Frequency = c(0.5, 0.5, 0.6, 0.4, 0.3, 0.7)
  )
  pops <- do.call(rbind, lapply(LETTERS[1:5], function(p) {
    d <- make_population(freqs, N = 30)
    d$Population <- p
    d
  }))
  mv     <- to_mv(pops[, setdiff(names(pops), "Population")])
  groups <- factor(pops$Population)
  list(graph = popgraph(mv, groups), data = mv, groups = groups)
}
