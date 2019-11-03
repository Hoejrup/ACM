% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rbi_norm.R
\name{rbi_norm}
\alias{rbi_norm}
\title{rbi_norm}
\usage{
rbi_norm(N, rho)
}
\arguments{
\item{N}{number of observations}

\item{rho}{correlation efficient}
}
\value{
a 2 dimensional matrix of a bivariate normal distribution
}
\description{
Generates a bivariate normal distribution
}
\examples{
rbi_norm(1000, 0.7)

}