# Set Library -------------------------------------
library(dplyr)
library(readr)
library(Matrix)
library(glmnet)
library(tibble)


# Load and Prepare Data ---------------------------------------
BankChurners <- read_csv("BankChurners.csv")
colSums(is.na(BankChurners))
summary(BankChurners)

BankChurners2 <- BankChurners %>% 
  column_to_rownames(var = "CLIENTNUM") %>% 
  select(Attrition_Flag, where(is.numeric))
summary(BankChurners2)
x = model.matrix(Attrition_Flag~., BankChurners2)[,-1]
y = BankChurners2$Attrition_Flag
dim(x)
length(y)


# Lasso Logistic Regression --------------------------------
build_logit_model <- function(alpha, x, y){
  grid =10^seq(10,-2, length=100)
  grid = c(grid,0)
  model = glmnet(x, y, alpha=alpha, family="binomial", lambda=grid)
  plot(model)
  return(model)
}

best_lamb_logit <- function(alpha, x, y, k){
  folds = sample(1:k, length(y), replace=TRUE)
  grid = 10^(seq(-3,0.1, length.out=1000))
  cv.acc = matrix(rep(0,length(grid)*k), k, length(grid))
  
  for(j in 1:k){
    model = glmnet(x[folds!=j,], y[folds!=j], alpha=alpha, family="binomial", lambda=grid)
    pred = predict(model, s=grid, newx = x[folds==j,], type="class")
    cv.acc[j,]= colMeans(pred == y[folds==j])
    
  }
  acc = colMeans(cv.acc)
  bestacc = acc[which.max(colMeans(cv.acc))]
  bestlamb = grid[which.max(colMeans(cv.acc))]
  plot(grid, acc*100, type='b')
  return(list(bestlamb, bestacc))
}

# Lasso regression -> alpha=1

## Fit Model --------------------------------------------
lasso_model = build_logit_model(alpha=1, x=x, y=y)

## Find Best Lambda -------------------------------------
bestlamb_result = best_lamb_logit(alpha=1, x=x, y=y, k=10)
lasso_bestlamb <- bestlamb_result[1]
bestacc <- bestlamb_result[2]
lasso_bestlamb
bestacc


## Coefficient of Best Model ----------------------------
predict(lasso_model, s=lasso_bestlamb, type="coefficients")[1:15,]



