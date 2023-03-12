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

## Change character variables to factor ------------------
sapply(BankChurners, class)
BankChurners2 <- BankChurners %>% mutate_if(is.character, as.factor)
sapply(BankChurners2, class)
summary(BankChurners2)

## Refactor reference level ------------------------
contrasts(BankChurners2$Attrition_Flag)
# The objective is to predict attrited customer, so I change it. 
BankChurners2$Attrition_Flag <- relevel(BankChurners2$Attrition_Flag, ref = 2)
contrasts(BankChurners2$Attrition_Flag)

## Select numerical and Attrition_Flag -----------------
BankChurners2 <- BankChurners2 %>% 
  column_to_rownames(var = "CLIENTNUM") %>% 
  select(where(is.numeric), Attrition_Flag)
summary(BankChurners2)


# Train/Test Splitting --------------------------------------------
# Using train/validate/test = 70/15/15
# For trainset -> use both train and validate (k-fold) later
sample <- sample(c(TRUE, FALSE), nrow(BankChurners2), replace=TRUE, prob=c(0.7,0.3))
train_set  <- BankChurners2[sample, ]
test_set   <- BankChurners2[!sample, ]

## Assign x and y for training ----------------------
x = model.matrix(Attrition_Flag~., train_set)[,-1]
y = train_set$Attrition_Flag
dim(x)
length(y)

## Assign newx for test -------------------------
x_test = model.matrix(Attrition_Flag~., test_set)[,-1]
y_test = test_set$Attrition_Flag
dim(x_test)
length(y_test)


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
bestlamb <- bestlamb_result[[1]]
bestacc <- bestlamb_result[[2]]
bestlamb
bestacc

## Coefficient of Best Model ----------------------------
predict(lasso_model, s=bestlamb, type="coefficients")

## Predict with test set ---------------------------------------
pred_test <- predict(lasso_model, s=bestlamb, type="class", newx = x_test)
conf_matrix <- table(pred_test, factor(y_test, levels=c("Attrited Customer", "Existing Customer")))
conf_matrix
acc_test = mean(pred_test == y_test)
fscore_test = 2*conf_matrix[1,1] / (2*conf_matrix[1,1] + conf_matrix[1,2] + conf_matrix[2,1])
acc_test
fscore_test



