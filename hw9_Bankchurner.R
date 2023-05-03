# Set Library ------------------------------------------------------------------
library(dplyr)
library(tree)
library(randomForest)
library(caret)
library(readr)
library(plyr)
library(gbm)
library(xgboost)
library(archdata)
library(Ckmeans.1d.dp)

# Data Preparation -------------------------------------------------------------
BankChurners <- read_csv("BankChurners.csv")
summary(BankChurners)

## Convert data to right type ---------------------------------
BankChurners <- BankChurners %>% 
  select(-c(CLIENTNUM)) %>% 
  mutate_if(is.character, as.factor) %>% 
  relocate(Total_Revolving_Bal, .after = last_col())
summary(BankChurners)

## Dealing with unknown category ------------------------------
# There are Education_Level, Marital_Status, and Income_Category 
#   that have Unknown value, so I will replace with most frequent value.
BankChurners$Education_Level <- revalue(BankChurners$Education_Level, c("Unknown"="Graduate"))
BankChurners$Marital_Status <- revalue(BankChurners$Marital_Status, c("Unknown"="Married"))
BankChurners$Income_Category <- revalue(BankChurners$Income_Category, c("Unknown"="Less than $40K"))
summary(BankChurners)

## Split k-fold CV --------------------------------------------
k = 10
set.seed(427)
folds = sample(1:k, nrow(BankChurners), replace=TRUE)


# Pruning Tree -----------------------------------------------------------------
cv.mse.prune = 1:k
for(j in 1:k){
  tree = tree(Total_Revolving_Bal~., BankChurners[folds!=j,])
  cv_tree = cv.tree(tree)
  prTree = prune.tree(tree, best=cv_tree$size[which.min(cv_tree$dev)])
  cv.mse.prune[j] = mean((BankChurners$Total_Revolving_Bal[folds==j] - 
                            predict(prTree, newdata=BankChurners[folds==j,]))^2)
}
rmse_prTree <- sqrt(mean(cv.mse.prune))
summary(prTree)
plot(prTree)
text(prTree, pretty = 0)


# Random Forest ----------------------------------------------------------------
# 19 predictors + 1 Total_Revolving_Bal
# It's large data, so using mtry=6 (Default is 19/3)
cvv = 1:k
for(j in 1:k){
  rf = randomForest(Total_Revolving_Bal~., data=BankChurners[folds!=j,], 
                    mtry=6, ntree=50, importance =TRUE)
  cvv[j] = mean((BankChurners$Total_Revolving_Bal[folds==j] - 
                   predict(rf, newdata=BankChurners[folds==j,]))^2)
}
rmse_rf <- sqrt(mean(cvv))


# Boosting ---------------------------------------------------------------------
## Default Parameters -----------------------------------------
cv.rmse.boost = 1:k
for(j in 1:k){
  boost = gbm(Total_Revolving_Bal~., data=BankChurners[folds!=j,], 
              distribution="gaussian", n.trees=50)
  cv.rmse.boost[j] = mean((BankChurners$Total_Revolving_Bal[folds==j]-
                             predict(boost, newdata=BankChurners[folds==j,], n.trees=50))^2)
}
rmse_boostDef <- sqrt(mean(cv.rmse.boost))

# I will not tuning parameters of Boosting because
#   1. The difference of RMSE from RF and Boosting is very much, so I don't think 
#      can tune parameter to perform better than RF.
#   2. It's use lots of time to build and tune model.


# XGBoost ----------------------------------------------------------------------
## Data Preparation -------------------------------------------
summary(BankChurners)
BankChurners_xgb <- BankChurners

# Convert categorical to numerical
# Attrition_Flag, Gender, Education_Level, Income_Category, Marital_Status, Card_Category
#   1. Convert Attrition_Flag, Gender to numeric now (only 2 options)
#   2. Reorder factor level of Education_Level, Income_Category, then convert to numerical
#   3. Not use Marital_Status, Card_Category because can't convert appropriate factor level

#1
BankChurners_xgb$Attrition_Flag = as.numeric(BankChurners_xgb$Attrition_Flag)
BankChurners_xgb$Gender = as.numeric(BankChurners_xgb$Gender)

#2
levels(BankChurners_xgb$Education_Level)
BankChurners_xgb$Education_Level <- factor(BankChurners_xgb$Education_Level, 
                                           levels=c('Uneducated', 'High School', 'College', 
                                                    'Graduate', 'Post-Graduate', 'Doctorate'))
levels(BankChurners_xgb$Income_Category)
BankChurners_xgb$Income_Category <- factor(BankChurners_xgb$Income_Category, 
                                           levels=c('Less than $40K', '$40K - $60K', '$60K - $80K',
                                                    '$80K - $120K', '$120K +'))
BankChurners_xgb$Education_Level = as.numeric(BankChurners_xgb$Education_Level)
BankChurners_xgb$Income_Category = as.numeric(BankChurners_xgb$Income_Category)

#3
BankChurners_xgb <- BankChurners_xgb %>% select(where(is.numeric))
summary(BankChurners_xgb)

# data_variables <- as.matrix(Auto_xgb[,-ncol(Auto_xgb)])
# data_label <- Auto_xgb[,ncol(Auto_xgb)]
# data_matrix <- xgb.DMatrix(data = as.matrix(data_variables), label = data_label)

data_variables <- as.matrix(BankChurners_xgb[,-ncol(BankChurners_xgb)])
data_label <- BankChurners_xgb[,ncol(BankChurners_xgb)]
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables), label=data_label$Total_Revolving_Bal)

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
  train_label  <- data_label$Total_Revolving_Bal[folds!=j]
  train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
  
  # split test data and make xgb.DMatrix
  test_data  <- data_variables[folds==j,]
  test_label <- data_label$Total_Revolving_Bal[folds==j]
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

# The best RMSE is from XGBoost method. I will not tuning parameters because
#   1. This RMSE is very better than others.
#   2. It's use lots of time to build and tune model.

### Feature Importance ---------------------
train_data   <- data_variables
train_label  <- data_label$Total_Revolving_Bal
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)

names <- colnames(BankChurners_xgb[,-ncol(BankChurners_xgb)])
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
gp = xgb.ggplot.importance(importance_matrix)
print(gp) 
