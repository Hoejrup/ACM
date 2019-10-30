#' ARsim
#'
#' The function ARsim() draws from the random variable, which is specified by the density f, using acceptance rejection sampling. 
#' The default proposal density is the exponential density with rate 1. If you want to use a different density you have to set the argument "exponential" to FALSE and specify your new proposal density and the corresponding inverse distribution function.
#'
#' @param f target density
#' @param C constant such that f(x) <= C*g(x)
#' @param N number of samples
#' @param exponential indicates if we want to use the exponential density as proposal
#' @param rate corresponding rate
#' @param propdensity alternative proposal if needed
#' @param inveresepropdist corresponding inverse distribution function
#'
#' @return returns a list containing the value of the integral, the 0.95-confidence interval, number of simulations and the function f
#'
#'
#' @examples
#' C <- 1.6
#' lambda <- 0.5
#' f <- function(x) {x*exp(-x)}
#' 
#' ARsimulation <- ARsim(f, C, 100000, rate = lambda)
#' 
#' @export

ARsim <- function(f, C, N, exponential = TRUE, rate = 1, propdensity = NA, inveresepropdist = NA){
  if(exponential == TRUE){
    expdensity <- function(x){exp(-rate*x)}
    sum <- 0
    Y <- rep(0,N)
    while(sum < N){
      X <- rexp(1,rate = rate)
      U <- runif(1)
      if(f(X)/(C*expdensity(X))>U){
        sum <- sum + 1
        Y[sum] <- X
      }
    }
    structure(list(samples = Y, "number of simulations" = N, "proposal density" = expdensity, 
                   "target density" = f),class = c("AR", "simulation"))
  } else{
    sum <- 0
    Y <- rep(0,N)
    while(sum < N){
      U_X <- runif(1)
      X <- inveresepropdist(U_X)
      U <- runif(1)
      if(f(X)/(C*propdensity(X))>U){
        sum <- sum + 1
        Y[sum] <- X
      }
    }
    structure(list(samples = Y, "number of simulations" = N, "proposal density" = propdensity,
                   "target density" = f),class = c("AR", "simulation"))
  }
  
}