
#' plot.AR
#'
#' The function plot.ARs() plots the target vs. the proposal density and the empirical vs. the theoretical density of the samples.
#' 
#' @param ARsample samples created by acceptance rejection
#'
#'
#' @examples
#' C <- 1.6
#' lambda <- 0.5
#' f <- function(x) {x*exp(-x)}
#' 
#' ARsimulation <- ARsim(f, C, 100000, rate = lambda)
#' plot(ARsimulation)
#' @export

plot.AR <- function(ARsample){
  y <- 0
  if("ggplot2" %in% rownames(installed.packages()) && "gridExtra" %in% rownames(installed.packages())){
    y <- menu(c("Yes", "No"), title="Do you want to use ggplot for plotting?")
  } 
  if(y == 1){
    x <- seq(min(ARsample$samples),max(ARsample$samples),0.01)
    data1 <- data.frame(x = x, y = ARsample$"proposal density"(x), z = ARsample$"target density"(x))
    data2 <- data.frame(x = ARsample$samples)
    plot1 <- ggplot2::ggplot(data1, ggplot2::aes(x)) + 
      ggplot2::geom_line(ggplot2::aes(y = y, colour = "proposal density")) + 
      ggplot2::geom_line(ggplot2::aes(y = z, colour = "target density")) + 
      ggplot2::ylab("proposal vs. target density") + 
      ggplot2::scale_colour_manual("", values = c("proposal density" ="green", "target density"="red"))
    plot2 <- ggplot2::ggplot(data2, ggplot2::aes(x)) + 
      ggplot2::stat_density(ggplot2::aes(colour = "empirical target density"),geom="line") + 
      ggplot2::geom_line(data = data1, ggplot2::aes(x, y = z, colour = "theoretical target density")) + 
      ggplot2::ylab("empirical vs. theoretical density") + 
      ggplot2::scale_colour_manual("", breaks = c("empirical target density", "theoretical target density"), values = c("empirical target density" ="green", "theoretical target density"="red"))
    gridExtra::grid.arrange(plot1, plot2, ncol=2)
  } else{
  par(mfrow = c(1,2))
  x <- seq(min(ARsample$samples),max(ARsample$samples),0.01)
  plot(x, ARsample$"proposal density"(x), type = "l", main = "target vs. proposal", 
       xlab = "", ylab = "")
  lines(x, ARsample$"target density"(x), col = "red")
  plot(density(ARsample$samples), main = "theoretical vs empirical proposal", xlab = "", ylab = "")
  lines(x, ARsample$"target density"(x), col = "red")
  }
}