#' rbi_norm
#'
#' Generates a bivariate normal distribution
#'
#' @param N number of observations
#' @param rho correlation efficient
#' @param mean is the mean of the normal distribution. If left empty, 0 by default
#' @param var is the variance of the normal distribution. If left empty, 1 by default
#' 
#'
#' @return a 2 dimensional matrix of a bivariate normal distribution 
#'
#'
#' @examples
#' rbi_norm(1000, 0.7)
#' 
#' @export

rbi_norm <- function(N=10^5, rho, mean=0, var=1){
  x <- matrix(rnorm(2*N, mean, var), ncol=2)  
  x[,2] <- rho*x[,1] + sqrt(1-rho^2)*x[,2]  
  return(x)
}

