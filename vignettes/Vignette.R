## ---- echo = FALSE-------------------------------------------------------
library(ACM)

## ------------------------------------------------------------------------
C <- 1.6
lambda <- 0.5
f <- function(x) {x*exp(-x)}

ARsimulation <- ARsim(f, C, 100000, rate = lambda)

## ---- fig.width = 8, fig.asp = .62---------------------------------------
plot(ARsimulation, ggplot = TRUE)
summary(ARsimulation)

## ------------------------------------------------------------------------
Finv <- function(x){-log(1-x)}
F_dist <- function(x){1-exp(-x)}

inv_samples <- inv_sample(Finv, N = 10000)
inv_samples_withF <- inv_sample(Finv, N = 10000, F_distribution = F_dist)

summary(inv_samples_withF)

## ------------------------------------------------------------------------
plot(inv_samples, ggplot = TRUE)

## ---- fig.width = 5, fig.asp = .62---------------------------------------
plot(inv_samples_withF, ggplot = TRUE)

## ------------------------------------------------------------------------
f <- function(x){x^2}
MCint <- int(f, -2, 2)
MCint$'Value of integral'

## ------------------------------------------------------------------------
(theoretical_value <- 16/3)

## ------------------------------------------------------------------------
summary(MCint)
plot(MCint, ggplot = TRUE)

## ------------------------------------------------------------------------
#Example using 1-Fréchet
N <- 10000
R <- -1/log(runif(N))
U <- runif(N)
X <- R*U
Y <- R*(1-U)
data <- matrix(c(X,Y),ncol=2)

## ------------------------------------------------------------------------
class(data) <- 'copula'
plot(data, ggplot=T)

## ------------------------------------------------------------------------
x <- MC_pi
plot(x, ggplot = T)

## ------------------------------------------------------------------------
data <- rbi_norm(N=1000, rho=0.7)

## ------------------------------------------------------------------------
x <- MC_pi
summary(x, print = F)

