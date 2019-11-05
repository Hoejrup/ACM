#' plot.simulation
#' 
#' @param x A result vector from a simulation algorithm
#' 
#' @return A plot of the summary of the input vector, containing mean, variance, relative error and confidence interval
#' 
#' @examples 
#' x <- MC_pi
#' class(x) <- 'simulation'
#' plot(x)
#' 
#' @export
plot.simulation <- function(x){
  summ <- summary(x)
  my_mean <- summ$Mean
  my_REL <- summ$`Relative Error`
  lower <- my_mean*(1-my_REL)
  upper <- my_mean*(1+my_REL)
  
  plot(1, my_mean, ylim = c(lower*(1-my_REL), upper*(1+my_REL)))
  points(c(1,1), c(lower,upper), col = 'blue')
}