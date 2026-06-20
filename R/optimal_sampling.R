#' Estimate optimal sampling allocation
#'
#' Returns a data frame of the variance in the interclass correlation
#' parameter (Phi) across all integer factorizations of \code{N} strata ×
#' individuals, together with a ±10 \% confidence band around the supplied
#' \code{phi} estimate. Pass the result directly to \code{ggplot2}.
#'
#' @param N The total sample size (number of strata × individuals per stratum).
#' @param phi An estimate of the expected level of differentiation.
#' @return A data frame with columns \code{Strata}, \code{Var.Phi},
#'   \code{Var.Phi.Low}, and \code{Var.Phi.High}.
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' df <- optimal_sampling(400, 0.25)
#' head(df)
#' \donttest{
#' library(ggplot2)
#' ggplot(df, aes(x = Strata, y = Var.Phi)) +
#'   geom_ribbon(aes(ymin = Var.Phi.Low, ymax = Var.Phi.High), alpha = 0.3) +
#'   geom_line() +
#'   labs(x = "Number of Strata",
#'        y = "Variance in Interclass Correlation Parameter")
#' }
optimal_sampling <- function(N, phi) {
  if (missing(N))
    stop("You need to provide the total sample size for this function to work.")
  if (missing(phi))
    stop("You need to provide a mean estimate of differentiation for this function to work.")

  x     <- seq(2, N - 2)
  y     <- N / x
  toUse <- (y == as.integer(y))
  K     <- y[toUse]
  J     <- N / K

  var.phi  <- 2 * ((1 + (K - 1) * phi)^2    * (1 - phi)^2)    / (K * (K - 1) * (J - 1))
  var.low  <- 2 * ((1 + (K - 1) * phi * .9)^2 * (1 - phi * .9)^2) / (K * (K - 1) * (J - 1))
  var.high <- 2 * ((1 + (K - 1) * phi * 1.1)^2 * (1 - phi * 1.1)^2) / (K * (K - 1) * (J - 1))

  data.frame(
    Strata       = J,
    Var.Phi      = var.phi,
    Var.Phi.Low  = var.low,
    Var.Phi.High = var.high
  )
}
