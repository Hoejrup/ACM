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
#' class(data) <- 'copula'
#' plot(data, ggplot=T)
#' 
#' 
#' @export

#plot copula using ECDF
plot.copula=function(data, ggplot = FALSE){
  xo=rank(data[,1])  
  yo=rank(data[,2])  
  n=length(data[,1])
  if(ggplot == T){
    data <- data.frame(x = data[,1], y = data[,2])
    ggplot2::ggplot(data=data, ggplot2::aes(xo/n, yo/n)) + 
      ggplot2::geom_jitter(color = 'Black', size=0.1) +
      ggplot2::xlab("Rank(X)/n") +
      ggplot2::ylab("Rank(Y)/n")
  }
  if(ggplot == F && "ggplot2" %in% rownames(installed.packages()) == T){
    y <- menu(c("Yes", "No"), title="You have installed ggplot2. Do you want to use it for plotting?")
  }
  if(y == 1){
    data <- data.frame(x = data[,1], y = data[,2])
    ggplot2::ggplot(data=data, ggplot2::aes(xo/n, yo/n)) + 
      ggplot2::geom_jitter(color = 'Black', size=0.1) +
      ggplot2::xlab("Rank(X)/n") +
      ggplot2::ylab("Rank(Y)/n")
  }
  else{
  plot(xo/n, yo/n, cex=0.1)
  }
}
