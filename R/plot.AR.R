#' plot.AR
#'
#' The function plot.ARs() plots the target vs. the proposal density and the empirical vs. the theoretical density of the samples.
#' 
#' @param ARsample samples created by acceptance rejection
#'
#'
#' @examples
#' C <- 1.6
#' lambda <- 0.5
#' f <- function(x) {x*exp(-x)}
#' 
#' ARsimulation <- ARsim(f, C, 100000, rate = lambda)
#' plot(ARsimulation)
#' @export

plot.AR <- function(ARsample){
  par(mfrow = c(1,2))
  x <- seq(min(ARsample$samples),max(ARsample$samples),0.01)
  plot(x, ARsample$"proposal density"(x), type = "l", main = "target vs. proposal", 
       xlab = "", ylab = "")
  lines(x, ARsample$"target density"(x), col = "red")
  plot(density(ARsample$samples), main = "theoretical vs empirical proposal", xlab = "", ylab = "")
  lines(x, ARsample$"target density"(x), col = "red")
}