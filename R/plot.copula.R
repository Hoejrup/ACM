#' plot.copula
#'
#' plot.copula plots a 2 dimensional copula, using empirical CDF.
#'
#' @param data 2 dimensional matrix
#'
#' @return plot of copula
#'
#'
#' @examples
#' N <- 1000
#' data <- rmult_norm(10000,rho=0.7)
#' class(data) <- 'copula'
#' plot(data)
#' 
#' @export

#plot copula
plot.copula=function(data){  
  xo=rank(data[,1])  
  yo=rank(data[,2])  
  n=length(data[,1])  
  plot(xo/n, yo/n, cex=0.1)
}
