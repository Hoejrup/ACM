#' summary.simulation
#' 
#' @param x A result vector from a simulation algorithm
#' 
#' @return A summary of the input vector, containing mean, variance, relative error and confidence interval
#' 
#' @export
#' 
summary.simulation <- function(x){
  summary <- vector("list", length = 0)
  summary['Mean'] <- mean(x)
  summary['Variance'] <- var(x)
  summary['Relative Error'] <- qnorm(0.975)*sqrt(var(x)/length(x))/mean(x)
  summary['Confidence interval'] <- paste("[",
                                          mean(x)-qnorm(0.975)*var(x)/sqrt(length(x)),
                                          ";",
                                          mean(x)+qnorm(0.975)*var(x)/sqrt(length(x)),
                                          "]")
  print(summary)
}