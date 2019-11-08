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
  
  ans <- 0
  if("ggplot2" %in% rownames(installed.packages()) && "gridExtra" %in% rownames(installed.packages())){
    ans <- menu(c("Yes", "No"), title="Do you want to use ggplot for plotting?")
  } 
  if(ans == 1){
    y <- data.frame(
      ones <- c(1,1,1),
      values = c(my_mean, lower, upper),
      is_mean = c(1,0,0)
    )
    
    ggplot2::ggplot(data = y, ggplot2::aes(x = ones, y = values, color = is_mean)) +
      ggplot2::geom_point() + 
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "none") + 
      ggplot2::ylab('Value') +
      ggplot2::xlab("Simulation")
  } else{
    plot(1, my_mean, ylim = c(lower*(1-my_REL), upper*(1+my_REL)))
    points(c(1,1), c(lower,upper), col = 'blue')
  }
}