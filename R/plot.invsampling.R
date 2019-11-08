#' plot.invsample
#'
#' The function plot.invsample() plots the empirical distribution function of the sampled values
#' 
#'
#' @param inv_sample
#' @param ggplot indicates if you want to use ggplot; please note that ggplot has to be installed in this case
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

plot.invsample <- function(inv_sample, ggplot = FALSE){
  if(ggplot == TRUE){
    if(is.function(inv_sample$"distribution function")) {
      y <- data.frame(y = inv_sample$samples)
      ggplot2::ggplot(y) + 
        ggplot2::stat_ecdf(ggplot2::aes(y, colour = "ecdf")) + 
        ggplot2::stat_function(fun = inv_sample$"distribution function", ggplot2::aes(colour = "theoretical distribution")) + 
        ggplot2::scale_colour_manual("", values = c("ecdf" ="green", "theoretical distribution"="red")) +
        ggplot2::labs(x = "x", y = "ecdf vs. theoretical")
    } else {
      y <- data.frame(y = inv_sample$samples)
      ggplot2::ggplot(y) + 
        ggplot2::stat_ecdf(ggplot2::aes(x = y)) +
        ggplot2::labs(x = "x", y = "ecdf")
    }
  } else{
  if(inv_sample$"number of simulations" < 10){
    warning("number of simulations is to small")
  } else{
  y <- 0
  if("ggplot2" %in% rownames(installed.packages())){
    y <- menu(c("Yes", "No"), title="Do you want to use ggplot for plotting?")
  } 
  if(y == 1){
    if(is.function(inv_sample$"distribution function")) {
      y <- data.frame(y = inv_sample$samples)
      ggplot2::ggplot(y) + 
      ggplot2::stat_ecdf(ggplot2::aes(y, colour = "ecdf")) + 
      ggplot2::stat_function(fun = inv_sample$"distribution function", ggplot2::aes(colour = "theoretical distribution")) + 
      ggplot2::scale_colour_manual("", values = c("ecdf" ="green", "theoretical distribution"="red")) +
      ggplot2::labs(x = "x", y = "ecdf vs. theoretical")
    } else {
      y <- data.frame(y = inv_sample$samples)
      ggplot2::ggplot(y) + 
      ggplot2::stat_ecdf(ggplot2::aes(x = y)) +
      ggplot2::labs(x = "x", y = "ecdf")
    }
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
  }
}
  
 