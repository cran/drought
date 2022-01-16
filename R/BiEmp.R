#' Compute the bivariate empirical joint probability
#' @param X The vector of a monthly hydro-climatic variable of n years(e.g., August).
#' @param Y The vector of a monthly hydro-climatic variable of n years(e.g., August).  
#' @return The empirical joint probability of X and Y for a specific month (Gringorten plotting position)
#' @export
#' @examples
#' X=runif(20, min = 0, max = 100) # 20 monthly values (e.g., August)
#' Y=runif(20, min = 0, max = 100)
#' fit<-BiEmp(X,Y) 


BiEmp<-function (X,Y)
  
{
X=matrix(X,ncol=1)
Y=matrix(Y,ncol=1)

n=length(X)

Z=matrix(NA,nrow=length(X),ncol=1)
 
for  (k in 1:n)
{  
  
  
  Z[k]=sum((X<=X[k])&(Y<=Y[k])) 
  
  Z[k]=(Z[k]-0.44)/(n+0.12)
  
} 

return(Z)

}