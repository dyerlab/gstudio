# Every node of 'graph' must be a level of 'groups'; otherwise resampled graphs
# can never be matched back to the observed edges.
#' @importFrom igraph V
#' @keywords internal
#' @noRd
.validate_asymmetry_groups <- function(graph, groups) {
  missing_nodes <- setdiff(igraph::V(graph)$name, unique(as.character(groups)))
  if (length(missing_nodes))
    stop("'groups' has no individuals for these graph nodes: ",
         paste(missing_nodes[seq_len(min(10, length(missing_nodes)))],
               collapse = ", "),
         if (length(missing_nodes) > 10) ", ..." else "",
         ". 'groups' must be the stratum variable used to build 'graph'.")
  invisible(TRUE)
}
