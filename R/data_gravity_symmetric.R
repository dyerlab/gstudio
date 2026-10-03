#' Example Population Graph under symmetric migration (Genetic Gravity manuscript)
#'
#' A Population Graph from the individual-based simulations of the Genetic Gravity
#' manuscript (Dyer, in prep.), chosen as the clearest example of the symmetric null:
#' strong isolation by distance but no directional gene flow.  Pair it with
#' \code{\link{gravity_redistributed}}.
#'
#' @format An undirected, weighted \code{popgraph}/\code{igraph} with 25 nodes and 72 edges.
#' \describe{
#'   \item{Edge attribute \code{weight}}{Conditional genetic distance (cGD).}
#'   \item{Vertex attribute \code{deme}}{Position along the one-dimensional stepping stone,
#'     1 (upstream end) to 25.  Use it as the covariate \code{x} in
#'     \code{\link{source_sink_test}} and \code{\link{directional_ibgd}}.}
#'   \item{Graph attributes}{\code{scenario} (\code{"Symmetric"}), \code{replicate} (5),
#'     \code{generation} (2804), \code{m_fwd} and \code{m_rev} (0.025 each), \code{source}.}
#' }
#'
#' @details
#' \strong{Simulation.} 25 demes of \eqn{N = 100} diploids scored at 20 biallelic loci on a
#' one-dimensional stepping stone; 2,000 generations of symmetric migration
#' (\eqn{m = 0.025} to each neighbour) followed by 1,000 generations of the
#' \emph{Symmetric} scenario (the same matrix).  This is lineage 5 at generation 2804.  The
#' Population Graph was built with \code{\link{popgraph}} from that generation's genotypes.
#'
#' \strong{Selection rule.} Among the 1,000 \emph{Symmetric} forward-phase censuses of the
#' manuscript's test grid (50 lineages, every 50 generations), those in which neither test
#' detected direction (source-sink test \eqn{p \ge 0.05} and \eqn{\Delta R^2 \le 0}, both at
#' \eqn{\gamma = 1/2}) and the graph is connected (937 censuses), the one with the strongest
#' isolation by graph distance (highest \eqn{R^2_{cGD}}).
#'
#' \strong{Values} (\code{gamma = 0.5} unless stated; test with \code{set.seed(1)},
#' \code{nperm = 999}): \eqn{R^2_{cGD} = 0.957}; source-sink test \eqn{r(S, x) = -0.09},
#' \eqn{p = 0.54}; \eqn{\Delta R^2 = -0.24}; degree share 0.04 at \eqn{\gamma = 1/2}
#' against 0.64 at \eqn{\gamma = 1} (the local mean), which is the degree floor the
#' degree-neutral bandwidth removes.
#'
#' @source Simulations of the Genetic Gravity manuscript (R. J. Dyer).
#' @seealso \code{\link{gravity_redistributed}}, \code{\link{source_sink_scores}},
#'   \code{\link{plot_gravity_field}}
#' @examples
#' data(gravity_symmetric)
#' gravity_field(gravity_symmetric)
#' set.seed(1)
#' source_sink_test(gravity_symmetric, x = igraph::V(gravity_symmetric)$deme)
#' @name gravity_symmetric
#' @docType data
#' @keywords data
NULL
