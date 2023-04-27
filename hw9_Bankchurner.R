# Set Library ------------------------------------------------------------------
library(dplyr)
library(tree)
library(randomForest)
library(caret)
library(readr)
library(plyr)

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


