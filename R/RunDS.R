#' Compute drought duration and severity based on run theory
#' 
#' The input data is monthly drought indices.
#' Duration is defined as the length of consecutive time series 
#' when drought index is below the threshold value (e.g., -1). 
#' Severity is defined as the summation of drought index below the threshold.
#' This analysis based on run theory is also referred to as threshold level method.
#' Here the standardized drought index (SDI) is used as 
#' the example to compute the drought characteristics. 
#' Other univariate and multivariate drought indices can also be used.
#' 
#' @param DI The vector of the drought index (e.g., monthly SPI)
#' @param thre The threshold of drought index (e.g, -0.5,-1)
#' @usage RunDS(DI, thre)
#' @references Yevjevich V. (1967). An Objective Approach to Definitions and Investigations of  Continental Hydrologic Droughts. Hydrology Paper 23. Colorado State University, Fort Collins, CO.
#' @return The duration and severity of each drought event 
#' @export
#' @examples
#' X=runif(120, min = 0, max = 100) # 10-year monthly data
#' thre=-1 # specify the threshold value
#' fit<-SDI(X,ts=3)  # Compute the univariate drought index, such as SPI
#' z=matrix(t(fit$SDI),ncol=1) # Reshape the matrix to a vector
#' Res <- RunDS(z, thre)  # Compute the duration and severity 
  RunDS <- function (DI, thre)
    {
  #  Get a vector with values (0,1) if the drought index is below the threshold or not
  X0 <-  rep(0,length(DI))
  X0[DI < thre] <- 1
  X1 <- c(0,X0,0)
  
  # Get the start time and end time of the each drought event
  st <- c()
  ed <- c()
  for (i in 2:(length(X1)-1)){
    if (X1[i-1] == 0 & X1[i] == 1){
      st <- c(st,i-1)
    }
    if (X1[i] == 1 & X1[i+1] == 0){
      ed <- c(ed,i-1)
    }
  }
  
  ## compute frequency, duration and severity of drought events
  Duration <-  ed-st+1
  Frequency <- length(Duration) # Can be computed based on duration
  Severity <- matrix(nrow=Frequency, ncol=1)
  for (i in 1:Frequency)
  {
    Severity[i,1] <- abs(sum(DI[st[i]:ed[i],1]-thre))
  }
  
  Result <- data.frame(Duration,Severity)
  return(Result)
  
}



