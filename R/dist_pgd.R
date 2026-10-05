#' Estimation of partitioned conditional genetic distance
#'
#' Builds the Population Graph as \code{dist_cgd()} does and returns all-pairs
#'  directed shortest-path partitioned conditional genetic distance (pGD; see
#'  \code{\link{pgd}}).  The matrix is asymmetric: \code{[i, j]} is the distance
#'  from \code{i} to \code{j}, shorter in the direction of gene flow.
#' @param x The genetic data as a \code{data.frame} with \code{locus} columns.
#' @param stratum The groups among which you are going to estimate genetic distances.
#' @param gamma Bandwidth multiplier for the neighbourhood weights (default
#'  \code{0.5}, degree-neutral); see \code{\link{source_sink_scores}}.
#' @return A square, asymmetric matrix of pGD estimates.
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @noRd
dist_pgd <- function( x, stratum="Population", gamma = 0.5 ) {
  if( !is( x, "data.frame") )
    stop("You need to pass a data.frame to dist_pgd() to work.")

  if( !(stratum %in% names(x)))
    stop("You need to specify the correct stratum for dist_pgd() to work.")

  mv <- to_mv( x )
  graph <- popgraph(x=mv, groups=factor( as.character( x[[stratum]] )))
  ret <- pgd(graph, gamma = gamma, output = "matrix")
  return(ret)
}
