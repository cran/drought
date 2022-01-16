\name{drought-package}
\alias{drought-package}
\alias{drought}
\docType{package}
\title{
Statistical Modeling and Assessment of Drought
}
\description{
Provide tools for drought monitoring based on univariate and multivariate drought indicators.Statistical drought prediction based on Ensemble Streamflow Prediction (ESP), drought risk assessments, and drought propagation are also provided. Please see Hao Zengchao et al. (2017) <doi:10.1016/j.envsoft.2017.02.008>.
}
\details{
\tabular{ll}{
Package: \tab drought\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
License: \tab GPL-3\cr
}

}

\references{

Hao, Z. et al. (2017), An integrated package for drought monitoring, prediction and analysis to aid drought modeling and assessment, Environ Modell Softw, 91, 199-209.

Hao, Z., and V. P. Singh (2015), Drought characterization from a multivariate perspective: A review J. Hydrol., 527

Hao and AghaKouchak (2013) Multivariate Standardized Drought Index: A parametric multi-index model, Advances in Water Resources 57, 12-18.

Kao, S. C. and R. S. Govindaraju (2010). A copula-based joint deficit index for droughts. Journal of Hydrology,  380(1-2): 121-134.  

Hao, Z. et al. (2014). Global integrated drought monitoring and prediction system. Scientific Data,  1 
}

\examples{
#' X=runif(120, min = 0, max = 100) #  10-year monthly data
#' Yc<-ACCU(X,ts=6) # Compute the 6 month  accumulated series
#' fit1<-SDI(X,ts=6) # Get the standardized drought index (or SPI)

#' z=matrix(t(fit1$SDI),ncol=1)
#' Res <- RunDS(z, -1)# Get drought duration and severity based on threshold SPI=-1

#' Y=runif(120, min = 0, max = 100)  #  10-year monthly data
#' fit2<-MSDI(X,Y,ts=6) # Compute the 6 month Multivariate Standardized Drought Index (MSDI)
#' fit2$MSDI #Get the empirical MSDI


#' PropagationMCC(X, Y, 12, c(-1,1))  # Plot drought propagation
}


