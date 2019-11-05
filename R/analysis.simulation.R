#' Simulation analysis
#'
#' Creates a simple PDF analysis for a simulation vector. PDF is saved in current working directory
#'
#' @param x A simulation vector
#'
#' @examples
#'
#' x <- MC_pi
#' class(x) <- 'simulation'
#' analysis(x)
#'
#' @import gridExtra
#' @export
#' 
analysis.simulation <- function(x){
  temp_plot <- plot(x)
  temp_summ <- as.data.frame(summary(x))
  
  pdf("plots.pdf", width = 5.83, height = 8.27)
  vp1 <- viewport(x = 0, y = 0.6, w = 1, h = 0.3,
                  just = c("left", "bottom"), name = "vp1")
  vp2 <- viewport(x = 0.1, y = 0, w = 0.8, h = 0.6,
                  just = c("left", "bottom"), name = "vp2")
  vp3 <- viewport(x = 0, y = 0.9, w = 1, h = 0.1,
                  just = c("left", "bottom"), name = "vp2")
  
  pushViewport(vp1)
  grid.table(temp_summ, rows = NULL)
  popViewport()
  pushViewport(vp2)
  grid.draw(as.grob(my_plot))
  popViewport()
  pushViewport(vp3)
  grid.text("Simulation analysis")
  popViewport()
  dev.off()
}