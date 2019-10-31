#' summary.MC_integral
#'
#' summary method for Monte Carlo integrals
#'
#' @export

summary.MC_integral <- function(int){
  paste("By Monte Carlo integration, the integral of the function f from",int$"lower bound","to", int$"upper bound","was evaluated, yielding the value", int$"Value of integral",".","The corresponding asymptotic 0.95-confidence interval is given by [",int$"asymptotic 95% confidence interval"[1],",",int$"asymptotic 95% confidence interval"[2],"]")
  }