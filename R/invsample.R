#' inv_sample
#'
#' The function inv_sample() draws from the random variable, which is specified by the distribution F, using inverse sampling. 
#' 
#'
#' @param Finv inverse distribution function
#' @param N number of samples
#' @param F_distribution distribution function, optional for more sophisticated plots
#'
#' @return returns a list containing the samples, the number of simulations and (optional) the distribution function.
#'
#' @examples
#' Finv <- function(x){-log(1-x)}
#' F_dist <- function(x){1-exp(-x)}
#'
#' inv_samples <- inv_sample(Finv, N = 10000)
#' inv_samples_withF <- inv_sample(Finv, N = 10000, F_distribution = F_dist)
#' inv_5samples <- inv_sample(Finv, N = 5)
#' 
#' @export

inv_sample <- function(Finv, N = 1, F_distribution = NA){
  U <- runif(N)
  structure(list(samples=Finv(U), "number of simulations"=N, "distribution function" = F_distribution),
            class = c("invsample", "simulation"))
}