### Main procedures

FGD_procedure <- function(X_train, y_train, eta = 0.01, nots = 30, method = 'spline') {
  
  models = list(); rho = c(); 
  steps = nrow(X_train)
  
  idx = sample(nrow(X_train),nrow(X_train))
  
  pred = matrix(NA, nrow = nrow(X_train),ncol = steps+1) # class prediction
  g = matrix(NA, nrow = nrow(X_train),ncol = steps+1) # prediction under logit
  f = matrix(NA, nrow = ncol(X_train), ncol = steps+1) # functional argument
  u = matrix(NA, nrow = ncol(X_train), ncol = steps) # gradients
  
  f[,1] = rnorm(ncol(X_train), sd = 0.001)
  g[,1] = apply(X_train, 1, operator, beta = f[,1])
  
  pred[,1] = sapply(g[,1], function(x) ifelse(x >= 0, 1, -1))
  
  for (m in 1:steps) {
    
    # gradient calculation
    u[,m] = gradient(f[,m], X_train, y_train, loss = 'class', idx = idx[m])
    
    if (method == 'SGD'){
      # SGD fit
      fitted = u[,m]
    } 
    
    if (method == 'spline'){
      # gam model
      models[[m]] = fit.step(u[,m],nots,method)
      fitted = models[[m]]$fitted.values
    }
    
    if(method == 'tree'){
      # tree based
      models[[m]] = fit.step(u[,m],nots,method)
      fitted = predict(models[[m]])
    }
    
    
    # adding base learner
    f[,m+1] = f[,m] - eta*(1/sqrt(m))*fitted
    
    # Calculating A[f], this is the prediction under the logit transformation
    g[,m+1] = apply(X_train, 1, operator, beta = f[,m+1])
    
    # threshold step of 0.5 for classification 
    pred[,m+1] = sapply(g[,m+1], function(x) ifelse(x >= 0, 1, -1))
    
  }
  
  if (method == 'SGD'){
    
    final_model_sgd = rowMeans(f)
    final_model_land = f[,ncol(f)]
    
    pred_sgd = pred_fgd(X_train, final_model_sgd)
    perf_sgd = performance(pred_sgd,y_train)
    
    pred_land = pred_fgd(X_train, final_model_land)
    perf_land = performance(pred_land,y_train)
    
    return(list(f_sgd = final_model_sgd, perf_sgd = perf_sgd, pred = pred_sgd,
                f_land = final_model_land, perf_land = perf_land, pred = pred_land))
  } else {
    
    final_model = rowMeans(f)
    pred = pred_fgd(X_train, final_model)
    perf = performance(pred, y_train)
    
    return(list(f = final_model, perf = perf, pred = pred))
  }
  
}

pfr_procedure = function(y_train, X_train, bs = "ps", k = 30, peer = F, m = 2){
  
  y_train_fpr = ifelse(y_train == 1, 1, 0)
  
  colnames(X_train) = paste('func',seq(1, ncol(X_train)), sep = '_')
  rownames(X_train) = paste('id', seq(1,nrow(X_train)), sep = '_')
  
  train_fpr = list(label = y_train_fpr, func = X_train)
  
  if (peer == F){
    fit.lf <- pfr(label ~ lf(func, m=m, bs = bs, k = k), data=train_fpr, family = 'binomial')
  } else {
    fit.lf <- pfr(label ~ lf(func), data=train_fpr, family = 'binomial')
  }
  
  bhat.lf <- coef(fit.lf, n = ncol(X_train))
  
  pred = round(predict(fit.lf, newdata = train_fpr, type = 'response'))
  
  perf = performance(pred,y_train_fpr)
  
  return(list(model = bhat.lf, pred = pred, perf = perf, fit = fit.lf))
}

