#' Compute the Multivariate Standardized Drought Index (MSDI)
#' 
#' Based on a pair of monthly hydro-climatic variable (or corresponding marginals), 
#' the MSDI is computed using the joint distribution (parametric or nonparametric forms). 
#' The current version is based on the Gringorten plotting position.
#' It can be extended to higher dimensions, such as trivariate case including meteorological, 
#' agricultural, and hydrological droughts. For the high dimension case, the copula
#'  or vine copula method can be employed
#' 
#' 
#' @param X  is the vector of a monthly hydro-climatic variable of n years. 
#' @param Y  is the vector of a monthly hydro-climatic variable of n years.
#' @param ts is the accumulated time scale. 
#' @references Hao and AghaKouchak (2013) Multivariate Standardized Drought Index: A parametric multi-index model, Advances in Water Resources 57, 12-18.
#' @return The monthly MSDI series of different time scales (based on Gringorten plotting position)
#' @export
#' @examples
#' 
#' X=runif(120, min = 0, max = 100) # 10-year monthly data
#' Y=runif(120, min = 0, max = 100) # 10-year monthly data

#' fit<-MSDI(X,Y,ts=6) # Compute the 6 month drought index
#' fit$ProbEmp2 #Get the empirical drought index (e.g.,Gringorten plotting position )
#' 
MSDI<-function (X,Y,ts=6) 
{
   
  
  X=matrix(X,ncol=1)
  Y=matrix(Y,ncol=1)
  
  # Obtain the accumulation of the variable for a specific time scale (ts) 
  
  XA=ACCU(X,ts)
  YA=ACCU(Y,ts)
      
      #  define the SI=Y0 for each month
      #  define the empirical distribution
      
  P_emp=matrix(NA,nrow=length(X)/12,ncol=12,byrow=T)
  P_gam=matrix(NA,nrow=length(X)/12,ncol=12,byrow=T)

  Emp2=matrix(NA,nrow=length(X)/12,ncol=12,byrow=T)
  
  
  for  (k in 1:12)
  {
  
    mdx=XA[,k]
    mdy=YA[,k]
    
    # if the first number is NA, exclude the first number and then compute SPI
    
    if (is.na(mdx[1])==TRUE) 
    {
      xd=mdx[2:length(mdx)]
      yd=mdy[2:length(mdy)]
    

      Emp2[2:length(mdx),k]=BiEmp(xd,yd)
      
    }
    
    else
      # if the first number is not NA, take the whole month to compute SPI
    {  
      
      xd=mdx
      yd=mdy    
       
      Emp2[,k]=BiEmp(xd,yd)
      
    }
    
  }
  
  #result<-list(Probemp=P_emp,Probgam=P_gam,ProbEmp2=Emp2)
   
r=stats::qnorm(Emp2)
# result<-list(ProbEmp2=r)
result<-list(MSDI=r)
  return(result)
}
