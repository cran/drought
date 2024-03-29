% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ACCU.R
\name{ACCU}
\alias{ACCU}
\title{Obtain the accumulation of monthly hydro-climatic variables}
\usage{
ACCU(X, ts = 6)
}
\arguments{
\item{X}{The vector of monthly hydro-climatic variables of n years. ts is the accumulated time scale.}

\item{ts}{The accumulated time scale}
}
\description{
Obtain the accumulation of monthly hydro-climatic variables
}
\examples{
X=runif(120, min = 0, max = 100) # 10-year monthly data
Y<-ACCU(X,ts=3) # Compute the 3 month  accumulated series
}
