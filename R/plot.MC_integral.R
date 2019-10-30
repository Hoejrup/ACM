#' plot.MC_integral
#'
#' The function plot.MC_integral plots the integrand of a corresponding MC integration 
#'
#'
#' @param MC output of a Monte Carlo integration
#'
#'
#' @return 
#'
#'
#' @examples
#' f <- function(x){x^2}
#' MCint <- int(f, -2, 2)
#' plot(MCint)
#' 
#' @export

plot.MC_integral <- function(MC){
  x <- seq(MC$'lower bound',MC$'upper bound', 0.01)
  y <- MC$'integrand'(x)
  plot(x,y, type = "l", xlab = "x", ylab = "f(x)")
  polygon(x = c(x,MC$'upper bound', MC$'lower bound'),
          y = c(y,0,0),
          col = "grey")
}