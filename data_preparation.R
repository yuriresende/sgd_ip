library(tidyverse)

### Preliminary dataset operations

load(file = 'dataset.rda')


dataset %>% group_by(category) %>%
  summarise(obs = n(), mean_initial_credit = mean(`0`), mean_final_credit = mean(`1`)) %>%
  xtable(digits = 0)

X = dataset %>% select(-c(address,category,label)) %>% as.matrix()
y = dataset$label

# scaling with log
X = log(X)

# centering x
colmeansX = colMeans(X)
X = t(apply(X, 1, function(x, mean) x-mean, mean = colmeansX))
