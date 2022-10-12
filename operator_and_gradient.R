library(tidyverse)

### Operator and Gradient Computation

operator <- function(beta,x,h = 1/500) sum(beta*x*h)

gradient <- function(fhat,X,y,loss = 'l2', idx){
  
  grad = c()
  
  Af = operator(beta = fhat, x = X[idx,])
  
  if (loss == 'l2'){
    
    error = y-Af
    
  } else {
    
    error = -(y/(1+exp(y*Af)))
  }
  
  for (s in 1:ncol(X)){
    
    grad[s] = mean(c(X[idx,s])*error[idx])
    
  }
  
  return(grad)
}
