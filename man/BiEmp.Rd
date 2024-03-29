% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BiEmp.R
\name{BiEmp}
\alias{BiEmp}
\title{Compute the bivariate empirical joint probability}
\usage{
BiEmp(X, Y)
}
\arguments{
\item{X}{The vector of a monthly hydro-climatic variable of n years(e.g., August).}

\item{Y}{The vector of a monthly hydro-climatic variable of n years(e.g., August).}
}
\value{
The empirical joint probability of X and Y for a specific month (Gringorten plotting position)
}
\description{
Compute the bivariate empirical joint probability
}
\examples{
X=runif(20, min = 0, max = 100) # 20 monthly values (e.g., August)
Y=runif(20, min = 0, max = 100)
fit<-BiEmp(X,Y) 
}
