#' summary.simulation
#' 
#' @param x A result vector from a simulation algorithm
#' 
#' @return A summary of the input vector, containing mean, variance, relative error and confidence interval
#' 
#' @examples 
#' x <- MC_pi
#' class(x) <- 'simulation'
#' summary(x)
#' @export
#' 
summary.simulation <- function(x, print = T){
  summary <- vector("list", length = 0)
  summary['Mean'] <- round(x = mean(x), digits = 4)
  summary['Variance'] <- round(x = var(x), digits = 4)
  summary['Relative Error'] <- round(x = qnorm(0.975)*sqrt(var(x)/length(x))/mean(x), digits = 4)
  summary['Confidence interval'] <- paste("[",
                                          round(x = mean(x)-qnorm(0.975)*var(x)/sqrt(length(x)), digits = 4),
                                          ";",
                                          round(x = mean(x)+qnorm(0.975)*var(x)/sqrt(length(x)), digits = 4),
                                          "]")

  if(print == T){
    print(summary)
  }
}