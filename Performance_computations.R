library(tidyverse)
library(caret)

### Performance and predictive functions

performance = function(pred,y){
  
  fitted_df = data.frame(y = pred, id = seq(1, length(y)), type = 'pred')
  obs_df = data.frame(y = y, id = seq(1, length(y)), type = 'label')
  
  confMatrix = confusionMatrix(as.factor(fitted_df$y),as.factor(obs_df$y))
  
  return(confMatrix)
}

pred_fgd = function(X,f){
  
  g = apply(X, 1, operator, beta = f)
  
  # threshold step of 0.5 for classification 
  pred = sapply(g, function(x) ifelse(x >= 0, 1, -1))
  
  return(pred)
  
}

pred_fpr = function(X,y,model){
  
  y_fpr = ifelse(y == 1, 1, 0)
  
  colnames(X) = paste('func',seq(1, ncol(X)), sep = '_')
  rownames(X) = paste('id', seq(1,nrow(X)), sep = '_')
  
  newdata_fpr = list(label = y_fpr, func = X)
  
  pred_lf = round(predict(model, newdata = newdata_fpr, type = 'response'))
  pred_lf = ifelse(pred_lf == 1, 1, -1)
  
  return(pred_lf)
}

