#' summary.AR
#'
#' summary method for acceptance rejection sampling
#'
#' @export

summary.AR <- function(x){
  paste(x$'number of simulations', "samples from the random variable, specified by the target density via acceptance rejection sampling.")
}