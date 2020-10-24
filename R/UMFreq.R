#' Univariate and multivariate return period (Gumbel copula)
#' @param X are the drought properties or indices
#' @param Y are the drought properties or indices
#' @param EL is the average reocurrence time
#' @return The univariate and multivariate return period
#' @export
#' @examples
#' X=runif(120, min = 0, max = 100)
#' Y=runif(120, min = 0, max = 100)
#' fit<-UMFreq(X,Y,1)  
UMFreq<-function (X,Y,EL=1) 
{
  
  # Assume the mean interval EL=1 (or with annual maxima)

  n=length(X);
  # Compute the univariate return period

  UT1<-matrix(NA, nrow=n,ncol=1)
  UT2<-matrix(NA, nrow=n,ncol=1)
  
  pa<-MASS::fitdistr(X,"exponential")
  P1=stats::pexp(X,pa$estimate)
  UT1=1/(1-P1)*EL; # Exceedance return period
  
  
  pg<-MASS::fitdistr(Y,"gamma")
  P2=stats::pgamma(Y,shape=pg$estimate[1],rate=pg$estimate[2])
  UT2=1/(1-P2)*EL;# Exceedance return period
  

  
  # Compute the joint return period using "copula" package
  d=2
  u=copula::pobs(cbind(X,Y))
  
  ## maximum pseudo-likelihood 
  fit <-copula::fitCopula(copula::gumbelCopula(), u, method="mpl")
  theta<-stats::coef(fit)
 
  cop <- copula::gumbelCopula(theta, dim=2)
  
  copk <- copula::onacopulaL("Gumbel", list(theta, 1:2))
  
  MTA<-matrix(NA, nrow=n,ncol=1)
  MTO<-matrix(NA, nrow=n,ncol=1)
  MTK<-matrix(NA, nrow=n,ncol=1)
  
  for (i in 1:n)
  {
   
    P=copula::pCopula(c(P1[i],P2[i]),cop) 
      
    MTA[i]= 1/(1-P1[i]-P2[i]+P)*EL  
    MTO[i]= 1/(1-P)*EL  
    
    
    Kg <- copula::pK(P, cop=copk@copula, 2)
    
    
    MTK[i]= 1/(1-Kg)*EL;  
    
  
  }
  
  
  print("The Univariate and multivariate return period:  AND, OR, Kendall case")
  
  
  result<-list(UT1=UT1,UT2=UT2,MTA=MTA,MTO=MTO,MTK=MTK)
  
  return(result)
  
}
