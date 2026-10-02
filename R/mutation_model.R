#' Create a mutation model specification
#'
#' Creates an object describing a mutation model for use in forward-time
#' simulations. Three models are supported: Infinite Alleles (IAM),
#' K-Allele (KAM), and Stepwise Mutation (SMM).
#'
#' @param rate Per-allele mutation probability on \code{[0,1]}.
#' @param model One of \code{"iam"}, \code{"kam"}, or \code{"smm"}.
#' @param k Number of possible allele states. Required for \code{"kam"} and
#'   must be >= 2.
#' @return A list of class \code{"mutation_model"} with elements
#'   \code{rate}, \code{model}, and \code{k}.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' mm <- mutation_model(rate = 0.001, model = "iam")
#' mm <- mutation_model(rate = 0.001, model = "kam", k = 10)
mutation_model <- function(rate, model = c("iam", "kam", "smm"), k = NULL) {
  model <- match.arg(model)
  if (rate < 0 || rate > 1)
    stop("Mutation rate must be on [0,1].")
  if (model == "kam") {
    if (is.null(k))
      stop("k (number of allele states) is required for the KAM model.")
    if (k < 2)
      stop("k must be >= 2 for the KAM model.")
  }
  ret <- list(rate = rate, model = model, k = k)
  class(ret) <- "mutation_model"
  return(ret)
}
