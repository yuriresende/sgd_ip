### Fitting procedure for boosting

fit.step <- function(u,nots,method) {
  
  df = data.frame('u' = u, 'x' = seq(1,length(u)))
  
  if (method == 'spline'){
    model = mgcv::gam(u ~ s(x, k = nots, bs = 'cr'), data = df)
  }
  
  if (method == 'tree'){
    model = rpart(u ~ x, data = df, control = rpart.control(minbucket = 5, maxdepth = nots))
  }
  
  return(model) 
  
}
