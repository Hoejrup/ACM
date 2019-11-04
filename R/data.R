#' Indirect dependence for lurkplot
#'
#' A dataset containig simulated data. X and Y are dependant on each other, by the lurking variable Z.
#'
#' @format A data frame with 1000 rows and 3 variables:
#'
#' \describe{
#'   \item{my_X}{Linear dependent on Z, with slope 1.2 and intercept 4.3. Further it contains a centered normal error term, with variance 2.4.}
#'   \item{my_Y}{Linear dependent on Z, with slope 3.2 and intercept -2.1. Further it contains a centered normal error term, with variance 1.7.}
#'   \item{my_Z}{1000 realizations of standard normal.}
#' }
"MC_pi"