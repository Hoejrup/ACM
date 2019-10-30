#' plot.invsample
#'
#' The function plot.invsample() plots the empirical distribution function of the sampled values
#' 
#'
#' @param inv_sample
#' 
#' @return returns a plot of the empirical distribution function
#' 
#' @examples
#' Finv <- function(x){-log(1-x)}
#' F_dist <- function(x){1-exp(-x)}
#'
#' inv_samples <- inv_sample(Finv, N = 10000)
#' inv_samples_withF <- inv_sample(Finv, N = 10000, F_distribution = F_dist)
#' 
#' plot(inv_samples)
#' plot(inv_samples_withF)
#' 
#' @export

plot.invsample <- function(inv_sample){
  if(inv_sample$"number of simulations" < 10){
    warning("number of simulations is to small")
  } else{
    if(is.function(inv_sample$"distribution function")) {
      y <- inv_sample$samples
      plot(ecdf(y), main = "Empirical cumulative distribution function compared to theoretical")
      x <- seq(min(y), max(y), 0.01)
      lines(x, inv_sample$"distribution function"(x), col = "red")
    } else {
      y <- inv_sample$samples
      plot(ecdf(y), main = "Empirical cumulative distribution function")
    }
  }
}