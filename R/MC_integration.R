#' int
#'
#' The function int() performs a Monte Carlo integration of the function f.
#'
#'
#' @param f the integrand
#' @param a lower bound of integration
#' @param b upper bound of integration
#' @param n number of simulation, per default 10^5
#'
#' @return returns a list containing the value of the integral, the 0.95-confidence interval, number of simulations and the function f
#'
#'
#' @examples
#' f <- function(x){x^2}
#' MCint <- int(f, -2, 2)
#' MCint$'Value of integral'
#' 
#' @export

int <- function(f, a, b, n = 10^5){
  U <- runif(n)
  U_ab <- a + (b-a)*U
  sim <- f(U_ab)
  estimate <- (b-a)*sim
  value <- mean(estimate)
  halfwidth <- sd(estimate)/sqrt(n)*qnorm(0.975)
  confidence_interval <- c(value - halfwidth, value + halfwidth)
  structure(list("integrand" =f, "lower bound" = a, "upper bound" = b, "Value of integral" = value,
                 "asymptotic 95% confidence interval" = confidence_interval, "number of simulations" = n),
            class = c("MC_integral", "simulation"))
  
} 