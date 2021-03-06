% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BiEmp.R
\name{BiEmp}
\alias{BiEmp}
\title{Compute the bivariate empirical joint probability}
\usage{
BiEmp(X, Y)
}
\arguments{
\item{X}{The vector of a monthly hydro-climatic variable of n years.}

\item{Y}{The vector of a monthly hydro-climatic variable of n years.}
}
\value{
The empirical joint probability time scale
}
\description{
Compute the bivariate empirical joint probability
}
\examples{
X=runif(120, min = 0, max = 100)
Y=runif(120, min = 0, max = 100)
fit<-BiEmp(X,Y) 
}
