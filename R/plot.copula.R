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
#' #Example using bivariate normal
#' N <- 1000
#' data <- rmult_norm(10000,rho=0.7)
#' class(data) <- 'copula'
#' plot(data)
#' 
#' 
#' #Example using 1-Fréchet
#' N <- 10000
#' R <- -1/log(runif(N))
#' U <- runif(N)
#' X <- R*U
#' Y <- R*(1-U)
#' data <- matrix(c(X,Y),ncol=2)
#' plot(data,cex=0.1)
#' class(data) <- 'copula'
#' plot(data)
#' 
#' 
#' @export

#plot copula using ECDF
plot.copula=function(data, ggplot=F){  
  xo=rank(data[,1])  
  yo=rank(data[,2])  
  n=length(data[,1])
  if("ggplot2" %in% rownames(installed.packages()) == T){
    
  }
  else{
  plot(xo/n, yo/n, cex=0.1)
  }
}


if("ggplot2" %in% rownames(installed.packages()) == T){
  
}

