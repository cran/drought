% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JDSI.R
\name{JDSI}
\alias{JDSI}
\title{Compute Joint Drought Severity Index with joint distribution}
\usage{
JDSI(X, Y, ts = 6, type = 1)
}
\arguments{
\item{X}{is the vector of a monthly hydro-climatic variable of n years.}

\item{Y}{is the vector of a monthly hydro-climatic variable of n years.}

\item{ts}{is the accumulated time scale.}

\item{type}{is the method used to compute the JDSI (1 is Joint distribution and 2 is the Kendall function).}
}
\value{
The multivariate drought index based on the joint distribution or Kendall distribution
}
\description{
The JDSI can be computed based on joint distribution or kendall distribution
}
\examples{
X=runif(120, min = 0, max = 100) # 10-year monthly data
Y=runif(120, min = 0, max = 100) # 10-year monthly data
fit<-JDSI(X,Y,ts=6)  
z=matrix(t(fit$JDSI),ncol=1)
plot(z, type="l", col=1, lwd=2, lty=1, xlim=c(0,120),xlab="Time",ylab="JDSI")
}
\references{
Hao, Z. et al. (2017) An integrated package for drought monitoring, prediction and analysis to aid drought modeling and assessment, Environ Modell Softw, 91, 199-209.
}
