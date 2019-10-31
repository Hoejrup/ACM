#' rbi_norm
#'
#' Generates a bivariate normal distribution
#'
#' @param N number of observations
#' @param rho correlation efficient
#' 
#'
#' @return a 2 dimensional matrix of a bivariate normal distribution 
#'
#'
#' @examples
#' rbi_norm(1000, 0.7)
#' 
#' @export

rbi_norm <- function(N,rho){
  x <- matrix(rnorm(2*N), ncol=2)  
  x[,2] <- rho*x[,1] + sqrt(1-rho^2)*x[,2]  
  return(x)
}
