#' Simulation analysis
#'
#' Creates a simple PDF analysis for a simulation vector. 
#' PDF is saved in current working directory
#'
#' @param x A simulation vector
#'
#' @examples
#'
#' x <- MC_pi
#' simulation_analysis(x)
#'
#' @import gridExtra
#' @import ggplotify
#' @export

simulation_analysis <- function(x){
  if(class(x)!='simulation'){
    stop("Input class is not 'simulation'. Convert input to proper class and try agian.")
  }
  temp_plot <- plot(x)
  temp_summ <- as.data.frame(summary(x))
  
  pdf("plots.pdf", width = 5.83, height = 8.27)
  vp1 <- grid::viewport(x = 0, y = 0.6, w = 1, h = 0.3,
                  just = c("left", "bottom"), name = "vp1")
  vp2 <- grid::viewport(x = 0.1, y = 0, w = 0.8, h = 0.6,
                  just = c("left", "bottom"), name = "vp2")
  vp3 <- grid::viewport(x = 0, y = 0.9, w = 1, h = 0.1,
                  just = c("left", "bottom"), name = "vp2")
  
  grid::pushViewport(vp1)
  gridExtra::grid.table(temp_summ, rows = NULL)
  grid::popViewport()
  grid::pushViewport(vp2)
  grid::grid.draw(ggplotify::as.grob(temp_plot))
  grid::popViewport()
  grid::pushViewport(vp3)
  grid::grid.text("Simulation analysis")
  grid::popViewport()
  dev.off()
}