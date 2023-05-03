# Set Library ------------------------------------------------------------------
library(dplyr)
library(ISLR)
library(tree)
library(randomForest)
library(caret)
library(gbm)
library(xgboost)
library(archdata)
library(Ckmeans.1d.dp)



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
    rf = randomForest(mpg~., data=Auto[folds!=j,], mtry=m, ntree=1000, importance =TRUE)
    cvv[j] = mean((Auto$mpg[folds==j] - predict(rf, newdata=Auto[folds==j,]))^2)
  }
  cv.rmse.rf[m] = sqrt(mean(cvv))
}
rf_nPredict <- which.min(cv.rmse.rf)
rmse_rf <- min(cv.rmse.rf)


# Boosting ---------------------------------------------------------------------
## Default Parameters -----------------------------------------
# interaction.depth = 1
# shrinkage = 0.1
# bag.fraction = 0.5

cv.rmse.boost = 1:k
for(j in 1:k){
  boost = gbm(mpg~., data=Auto[folds!=j,], distribution="gaussian", n.trees=1000)
  cv.rmse.boost[j] = mean((Auto$mpg[folds==j]-
                                  predict(boost,newdata=Auto[folds==j,], n.trees=1000))^2)
}
rmse_boostDef <- sqrt(mean(cv.rmse.boost))

## Tuning Parameters -----------------------------------------
# interaction.depth, shrinkage, bag.fraction
idL = 1:3
shinkL = seq(0.01, 0.1, by=0.01)
bfracL = seq(0.3, 0.7, by=0.1)
boostTune_rmse_df = data.frame(matrix(ncol=4,nrow=0, 
                            dimnames=list(NULL, c("interactionDepth", "shrinkage", "bagFraction", "rmse"))))
for (id in 1:length(idL)){
  for (sh in 1:length(shinkL)){
    for (bf in 1:length(bfracL)){
      cv.errors = 1:k
      for (j in 1:k){
        boost = gbm(mpg~., data=Auto[folds!=j,], distribution="gaussian", n.trees=1000,
                    interaction.depth = idL[id],
                    shrinkage = shinkL[sh],
                    bag.fraction = bfracL[bf])
        cv.errors[j] = sqrt(mean((Auto$mpg[folds==j]-
                                        predict(boost,newdata=Auto[folds==j,], n.trees=1000))^2))
      }
      boostTune_rmse_df[nrow(boostTune_rmse_df) + 1,] = c(idL[id], shinkL[sh], bfracL[bf], mean(cv.errors))
    }
  }
}

rmse_boostTune <- min(boostTune_rmse_df$rmse)
boostTune <- boostTune_rmse_df[which.min(boostTune_rmse_df$rmse),]
# id_boostTune <- boostTune$interactionDepth
# sh_boostTune <- boostTune$shrinkage
# bf_boostTune <- boostTune$bagFraction


# XGBoost ----------------------------------------------------------------------
## Data Preparation -------------------------------------------
Auto_xgb <- Auto
data_variables <- as.matrix(Auto_xgb[,-ncol(Auto_xgb)])
data_label <- Auto_xgb[,ncol(Auto_xgb)]
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables), label = data_label)

## Default Parameters -----------------------------------------
# eta = 0.3
# max_depth = 6
# min_child_weight = 1
# subsample = 1

xgb_params <- list("objective" = "reg:squarederror",
                   "eval_metric" = "rmse")
nround <- 20
cv.rmse.xgboost = 1:k
for (j in 1:k){
  # split train data and make xgb.DMatrix
  train_data   <- data_variables[folds!=j,]
  train_label  <- data_label[folds!=j]
  train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
  
  # split test data and make xgb.DMatrix
  test_data  <- data_variables[folds==j,]
  test_label <- data_label[folds==j]
  test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
  
  # train model
  bst_model <- xgb.train(params = xgb_params,
                         data = train_matrix,
                         nrounds = nround)
  
  # Predict hold-out test set
  test_pred <- predict(bst_model, newdata = test_matrix)
  cv.rmse.xgboost[j] = sqrt(mean((test_label-test_pred)^2))
}

rmse_xgboostDef <- mean(cv.rmse.xgboost)

## Tuning Parameters -----------------------------------------
etaL = seq(0.2,0.4,0.05)
maxdepL = 4:6
mchwL = 1:3   
ssL = seq(0.5,1,0.1)
xgboostTune_rmse_df = data.frame(matrix(ncol=5,nrow=0, 
                                        dimnames=list(NULL, c("learningRate", "maxDepth", 
                                                              "minChildWeight", "subsample", "rmse"))))
for(eta in 1:length(etaL)){
  for(md in 1:length(maxdepL)){
    for(mchw in 1:length(mchwL)){
      for(ss in 1:length(ssL)){
        xgb_params <- list("eta" = etaL[eta], 
                           "max_depth" = maxdepL[md],
                           "min_child_weight" = mchwL[mchw],
                           "subsample" = ssL[ss],
                           "objective" = "reg:squarederror",
                           "eval_metric" = "rmse")
        nround <- 20
        
        cv.errors = 1:k
        for (j in 1:k){
          # split train data and make xgb.DMatrix
          train_data   <- data_variables[folds!=j,]
          train_label  <- data_label[folds!=j]
          train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
          
          # split test data and make xgb.DMatrix
          test_data  <- data_variables[folds==j,]
          test_label <- data_label[folds==j]
          test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
          
          # train model
          bst_model <- xgb.train(params = xgb_params,
                                 data = train_matrix,
                                 nrounds = nround)
          
          # Predict hold-out test set
          test_pred <- predict(bst_model, newdata = test_matrix)
          cv.errors[j] = sqrt(mean((test_label-test_pred)^2))
        }
        
        xgboostTune_rmse_df[nrow(xgboostTune_rmse_df) + 1,] = 
          c(etaL[eta], maxdepL[md], mchwL[mchw], ssL[ss], mean(cv.errors))
      }
    }
  }
}

rmse_xgboostTune <- min(xgboostTune_rmse_df$rmse)
xgboostTune <- xgboostTune_rmse_df[which.min(xgboostTune_rmse_df$rmse),]

### Feature Importance ---------------------
xgboost_lr <- xgboostTune$learningRate
xgboost_md <- xgboostTune$maxDepth
xgboost_mchw <- xgboostTune$minChildWeight
xgboost_ss <- xgboostTune$subsample

xgb_params_best <- list("eta" = xgboost_lr, 
                        "max_depth" = xgboost_md,
                        "min_child_weight" = xgboost_mchw,
                        "subsample" = xgboost_ss,
                        "objective" = "reg:squarederror",
                        "eval_metric" = "rmse")
nround <- 20

train_data <- data_variables
train_label <- data_label
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
bst_model <- xgb.train(params = xgb_params_best,
                       data = train_matrix,
                       nrounds = nround)

names <- colnames(Auto_xgb[,-ncol(Auto_xgb)])
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
gp = xgb.ggplot.importance(importance_matrix)
print(gp) 
