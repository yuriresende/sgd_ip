library(tidyverse)
library(caret)
library(rpart)

### Wrapper for fold validation

wrapper <- function(X, y, nfolds = 3){
  
  folds = createFolds(y, k = nfolds)
  metrics = list()
  
  out_perf_acc = matrix(NA, nrow = 6, ncol = nfolds)
  colnames(out_perf_acc) = paste('fold', seq(1,nfolds), sep = '_')
  rownames(out_perf_acc) = c('FGD_20','FGD_10','Tree', 'SGD', 'Bench1', 'Bench2')
  
  out_perf_kappa = matrix(NA, nrow = 6, ncol = nfolds)
  colnames(out_perf_kappa) = paste('fold', seq(1,nfolds), sep = '_')
  rownames(out_perf_kappa) = c('FGD_20','FGD_10','Tree', 'SGD', 'Bench1', 'Bench2')
  
  for (i in 1:nfolds){
    
    idx = folds[[i]]
    
    X_train = X[-idx,] 
    y_train = y[-idx]
    
    X_test = X[idx,]
    y_test = y[idx]
    
    fgd = FGD_procedure(X_train = X_train, y_train = y_train, eta = 10, nots = 20, method = 'spline')
    
    pred_ml_sgd = pred_fgd(X_test, fgd$f)
    perf_fgd = performance(pred_ml_sgd, y_test)
    
    fgd_10 = FGD_procedure(X_train = X_train, y_train = y_train, eta = 10, nots = 10, method = 'spline')
    
    pred_sgd_10 = pred_fgd(X_test, fgd_10$f)
    perf_fgd_10 = performance(pred_sgd_10, y_test)
    
    fgd_tree = FGD_procedure(X_train = X_train, y_train = y_train, eta = 10, nots = 20, method = 'tree')
    
    pred_tree = pred_fgd(X_test, fgd_tree$f)
    perf_tree = performance(pred_tree, y_test)
    
    sgd = FGD_procedure(X_train = X_train, y_train = y_train, eta = 10, method = 'SGD')
    
    pred_sgd = pred_fgd(X_test, sgd$f_sgd)
    perf_sgd = performance(pred_sgd, y_test)
    
    bench1 = pfr_procedure(X_train = X_train, y_train = y_train, k = 10, bs = "cr")
    bench2 = pfr_procedure(X_train = X_train, y_train = y_train, k = 20, bs = "cr")
    
    pred_bench1 = pred_fpr(X_test,y_test,bench1$fit)
    perf_bench1 = performance(pred_bench1, y_test)
    
    pred_bench2 = pred_fpr(X_test,y_test,bench2$fit)
    perf_bench2 = performance(pred_bench2, y_test)
    
    # out of sample evaluation
    out_perf_acc[,i] = c(perf_fgd$overall['Accuracy'], perf_fgd_10$overall['Accuracy'], 
                         perf_tree$overall['Accuracy'], perf_sgd$overall['Accuracy'], 
                         perf_bench1$overall['Accuracy'], perf_bench2$overall['Accuracy'])
    
    out_perf_kappa[,i] = c(perf_fgd$overall['Kappa'], perf_fgd_10$overall['Kappa'], 
                           perf_tree$overall['Kappa'], perf_sgd$overall['Kappa'], 
                           perf_bench1$overall['Kappa'], perf_bench2$overall['Kappa'])
    
    metrics[[i]] = list(fgd_train = fgd$perf, fgd_test = perf_fgd,
                        fgd_10_train = fgd_10$perf, fgd_10_test = perf_fgd_10,
                        fgd_tree_train = fgd_tree$perf, fgd_tree_test = perf_tree,
                        sgd_train = sgd$perf_sgd, sgd_test = perf_sgd,
                        bench1_train = bench1$perf, bench1_test = perf_bench1,
                        bench2_train = bench2$perf, bench2_test = perf_bench2)
    
    print(i)
    
  }
  
  results_comp = list(metrics = metrics, acc = out_perf_acc, kappa = out_perf_kappa)
  
  return(results_comp)
}

set.seed(2)

results = wrapper(X,y)

cv_results = data.frame(results$acc) %>% mutate(avg_accuracy = rowMeans(results$acc))

xtable(cv_results, digits = 2)
