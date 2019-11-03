#' summary.invsample
#'
#' summary method for inverse sampling
#'
#' @export

summary.invsample <- function(x){
  paste(x$'number of simulations', "samples from the random variable, specified by the inverse distribution via inverse sampling.")
}