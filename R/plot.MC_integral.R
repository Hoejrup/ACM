#' plot.MC_integral
#'
#' The function plot.MC_integral plots the integrand of a corresponding MC integration 
#'
#'
#' @param MC output of a Monte Carlo integration
#'
#'
#' @return 
#'
#'
#' @examples
#' f <- function(x){x^2}
#' MCint <- int(f, -2, 2)
#' plot(MCint)
#' 
#' @export

plot.MC_integral <- function(MC){
  y <- 0
  if("ggplot2" %in% rownames(installed.packages())){
    y <- menu(c("Yes", "No"), title="Do you want to use ggplot for plotting?")
  } 
  if(y == 1){
    x <- seq(MC$'lower bound',MC$'upper bound', 0.01)
    data <- data.frame(x = x, y = MC$'integrand'(x))
    ggplot2::ggplot(data=data, ggplot2::aes(x, y)) +
      ggplot2::geom_line() + 
      ggplot2::geom_ribbon(ggplot2::aes(ymin=0, ymax=y),fill = "grey70") +
      ggplot2::ylab("f(x)")
    } else{
    x <- seq(MC$'lower bound',MC$'upper bound', 0.01)
    y <- MC$'integrand'(x)
    plot(x,y, type = "l", xlab = "x", ylab = "f(x)")
    polygon(x = c(x,MC$'upper bound', MC$'lower bound'),
          y = c(y,0,0),
          col = "grey")
  }
}