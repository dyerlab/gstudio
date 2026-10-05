#' Simulated genotypes under directional gene flow (Genetic Gravity manuscript)
#'
#' Multilocus genotypes from one census of the individual-based simulations of
#' the Genetic Gravity manuscript (Dyer, in prep.): 25 populations on a
#' one-dimensional stepping stone in which migration toward higher-numbered
#' populations is four times migration in the reverse direction.  The direction
#' of gene flow is known, so the data are a worked example for the genetic
#' gravity analyses, from genotypes to a Population Graph, source-sink scores and
#' tests for a directional gradient (see \code{vignette("genetic_gravity")}).
#'
#' @format A \code{data.frame} with 2,500 rows (100 individuals in each of 25
#'   populations) and 22 columns:
#' \describe{
#'   \item{ID}{Simulation bookkeeping carried over from the simulation output;
#'     not a unique individual identifier (it takes only three values).}
#'   \item{Population}{Factor, \code{Pop01} to \code{Pop25}, numbered along the
#'     stepping stone from the upstream end (\code{Pop01}).  Gene flow is biased
#'     toward higher numbers.}
#'   \item{L01--L20}{Twenty biallelic loci, as \code{\link{locus}} objects.}
#' }
#'
#' @details
#' \strong{Simulation.} 25 demes of \eqn{N = 100} diploids scored at 20 biallelic
#' loci.  2,000 generations of symmetric stepping-stone migration
#' (\eqn{m = 0.025} to each neighbour) were followed by the \emph{Redistributed}
#' scenario: total migration unchanged (0.05) but split 4:1 toward
#' higher-numbered demes (\eqn{m_{i \to i+1} = 0.04},
#' \eqn{m_{i \leftarrow i+1} = 0.01}).  These are lineage 4 at generation 2504, in
#' the core of that lineage's \emph{plateau} phase, after the among-population
#' covariance has adjusted to the new migration matrix and before loss of
#' polymorphism erodes it.
#'
#' \strong{Selection rule.} Among the \emph{Redistributed} plateau-core censuses of
#' the manuscript's test grid (50 lineages, every 50 generations) in which both
#' directional tests detected the imposed direction at \eqn{\gamma = 1/2}
#' (source-sink test \eqn{p < 0.05} with sources upstream, and
#' \eqn{\Delta R^2 > 0} with a forward-to-reverse slope ratio below one) and the
#' graph is connected (92 censuses), the one with the strongest source-sink
#' gradient.
#'
#' \strong{Expected values.} \code{popgraph(to_mv(gravity), gravity$Population)}
#' has 25 nodes and 81 edges.  At \code{gamma = 0.5} (with \code{set.seed(1)},
#' \code{nperm = 999}): source-sink test \eqn{r(S, x) = -0.955}, \eqn{p = 0.001}
#' (sources upstream); \eqn{R^2_{cGD} = 0.67}, \eqn{\Delta R^2 = +0.16},
#' forward-to-reverse slope ratio 0.02; degree share 0.02 (0.11 at
#' \code{gamma = 1}).
#'
#' @source Simulations of the Genetic Gravity manuscript (R. J. Dyer):
#'   \code{AsymmetricPopGraphs/data/replicate4/rep4-genotypes-scenario2-2504.rda}.
#' @seealso \code{vignette("genetic_gravity")}, \code{\link{genetic_gravity}},
#'   \code{\link{gravity_field}}, \code{\link{source_sink_test}},
#'   \code{\link{ibgd}}
#' @examples
#' data(gravity)
#' graph <- popgraph(to_mv(gravity), gravity$Population)
#' deme  <- as.integer(sub("Pop", "", igraph::V(graph)$name))
#' set.seed(1)
#' source_sink_test(graph, x = deme)
#' ibgd(graph, x = deme, mode = "pgd")
#' \donttest{
#' plot(gravity_field(graph))
#' }
#' @name gravity
#' @docType data
#' @keywords data
NULL
