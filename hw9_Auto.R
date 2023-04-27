# Set Library ------------------------------------------------------------------
library(dplyr)
library(ISLR)
library(tree)
library(randomForest)
library(caret)


# Data Preparation -------------------------------------------------------------
Auto <- Auto
summary(Auto)
Auto <- Auto %>% 
  select(-c(name)) %>% 
  relocate(mpg, .after = last_col())
summary(Auto)

## Split k-fold CV -----------------------------------
k = 10
set.seed(427)
folds = sample(1:k, nrow(Auto), replace=TRUE)


# Pruning Tree -----------------------------------------------------------------
cv.mse.prune = 1:k
for(j in 1:k){
  tree = tree(mpg~., Auto[folds!=j,])
  cv_tree = cv.tree(tree)
  prTree = prune.tree(tree, best=cv_tree$size[which.min(cv_tree$dev)])
  cv.mse.prune[j] = mean((Auto$mpg[folds==j] - predict(prTree, newdata=Auto[folds==j,]))^2)
}
rmse_prTree <- sqrt(mean(cv.mse.prune))
# summary(auto_prTree)
# plot(auto_prTree)
# text(auto_prTree, pretty = 0)


# Random Forest ----------------------------------------------------------------
# 7 predictors + 1 mpg
cv.rmse.rf = 1:7
for (m in 1:7){
  cvv = 1:k
  for(j in 1:k){
    rf = randomForest(mpg~., data=Auto[folds!=j,], mtry=m, ntree=100, importance =TRUE)
    cvv[j] = mean((Auto$mpg[folds==j] - predict(rf, newdata=Auto[folds==j,]))^2)
  }
  cv.rmse.rf[m] = sqrt(mean(cvv))
}
rf_nPredict <- which.min(cv.rmse.rf)
rmse_rf <- min(cv.rmse.rf)


# Boosting ---------------------------------------------------------------------



# XGBoost ----------------------------------------------------------------------




