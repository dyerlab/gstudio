#' Estimate optimal sampling allocation
#' 
#' This function returns a graphical representation of optimal sampling
#'  allocations given an estimate of the total number of samples you 
#'  are looking at genotyping and the amount of expected differentiation
#'  among strata.
#' @param N The total sample size (number of strata * number of individuals 
#'  pre stratum)
#' @param phi An estimate of the level of differentiation.
#' @return A ggplot object with a plot of the variance in phi due to
#'  different sample sizes with a region of +/- 10% of the provided
#'  phi value
#' @export
#' @author Rodney J. Dyer \email{rjdyer@@vcu.edu}
#' @examples
#' N <- 400
#' Phi <- 0.25
#' optimal_sampling( N, Phi )
optimal_sampling <- function( N, phi) {
  if( missing(N) )
    stop("You need to provide the total sample size for this function to work.")
  
  if( missing( phi ) )
    stop("You need to provide a mean estiamte of differentiation for this function to work.")
  Strata <- Var.Phi <- NULL
  
  x <- seq(2,N-2)
  y <- N / x
  y1 <- as.integer(y)
  toUse <- y==y1
  K <- y[toUse]
  J <- N / K
  
  var.phi <- 2 * ( (1 + (K-1)*phi)^2 * (1-phi)^2)  / (  K*(K-1)*(J-1)  )

  phi.low <- phi * 0.9
  var.low <- 2 * ( (1 + (K-1)*phi.low)^2 * (1-phi.low)^2)  / (  K*(K-1)*(J-1)  )
  
  phi.high <- phi * 1.1
  var.high <-  2 * ( (1 + (K-1)*phi.high)^2 * (1-phi.high)^2)  / (  K*(K-1)*(J-1)  )  
  
  df <- data.frame(Strata=J, Var.Phi=var.phi )
  df1 <- data.frame( Strata=c(J,rev(J)), Var.Phi=c(var.low,rev(var.high)))
  ret <- ggplot( df, aes(x=Strata,y=Var.Phi))   
  ret <- ret + geom_polygon( data=df1, alpha=0.5 )
  ret <- ret + geom_line() + geom_point(size=4,aes(color=Var.Phi)) 
  ret <- ret + xlab("Number of Strata") + ylab("Variance in Interclass Correlation Parameter") 
  ret <- ret + scale_color_gradient( low="#78C679", high="#B30000", guide="none")

  return( ret )
}