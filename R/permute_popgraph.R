#' Bootstraps individuals to see stability of graph topology.
#'
#' This function uses a permutation test to look at edge stability.  What we 
#'  do is resample individuals and re-estimate the topology several times. This
#'  provides an estimate of edge stability.
#' @param data The raw multivariate data as submitted to \code{popgraph}
#' @param groups The grouping of the data into nodes as submitted to \code{popgraph}
#' @param nboot The nubmer of times to bootstrap the individuals per group (default=50)
#' @param ... Other arguments to be passed to \code{popgraph}
#' @return A weighted graph where edge weights represent the proportion of times the 
#'  edge was found in the perumuted data sets.
#' @export
#' 
permute_popgraph <- function( data, groups, nboot=50, ...){
  if( !is(data,"matrix"))
    stop("Cannot use non-matrix data to make a graph, let alone bootstrap it...")
  if( nrow(data) != length(groups))
    stop("You need to have data of the same size to use this function.")
  if( !is(groups,"factor"))
    groups <- factor(groups)
  
  strata <- levels(groups)
  K <- length(strata)
  A <- matrix(0,K,K)
  rownames(A) <- colnames(A) <- strata

  # Resample individuals WITH replacement, independently within each stratum
  # (a plain, per-stratum sample() rather than sampling::strata(method =
  # "srswr")). The old sampling::strata() call required its input sorted by
  # the stratification column and returned ID_unit as an index into that
  # SORTED order; those indices were then used to subset `data`/`df`
  # (unsorted) while pairing the result with the ORIGINAL, unsorted `groups`
  # vector -- a silent row-alignment bug whenever `data`/`groups` weren't
  # already stratum-sorted. Indexing within split(seq_along(groups), groups)
  # keeps the resampled rows and their group labels correctly paired by
  # construction, for any input order.
  strata_idx <- split(seq_along(groups), groups)

  for( rep in 1:nboot){
    idx <- unlist(lapply(strata_idx, function(ii) sample(ii, length(ii), replace = TRUE)),
                  use.names = FALSE)
    graph <- popgraph(data[idx, , drop = FALSE], groups[idx])
    B <- to_matrix(graph,mode = "adjacency")
    B[ B!=0 ] <- 1
    A <- A+B
    message(".", appendLF = FALSE)
    if( !(rep %% 50))
      message(" [", rep, "/", nboot, "]")
  }
  A <- A/nboot
  orig <- popgraph( data, groups )
  origA <- to_matrix(orig,mode = "adjacency")
  A <- A * origA
  
  graph <- decorate_graph(orig, A )
  return(graph)
}