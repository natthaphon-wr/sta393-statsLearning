# Set library ----------------------------------------
library(glmnet)

# Import and prepare data --------------------------
College = read.csv("College.csv",header=T,na.strings="?")
rownames(College) <- College[,1]
College = College[,-1]
# fix(College)
colSums(is.na(College))
dim(College)


# Linear regression model ----------------------------
x = model.matrix(Grad.Rate~.,College)[,-1]
y = College$Grad.Rate
dim(x)
length(y)

## Function ---------------------------------------
# alpha = 1 -> lasso, alpha=0 -> ridge
build_model <- function(alpha, x, y){
  grid = 10^seq(10,-2, length=100)
  grid = c(grid,0)
  model = glmnet(x, y, alpha=alpha, lambda=grid) 
  plot(model)
  return(model)
}

find_best_lamb <- function(alpha, model, x, y, k){
  cv.out = cv.glmnet(x, y, alpha=alpha, nfolds=k)
  best_lamb = cv.out$lambda.min
  plot(cv.out$lambda, cv.out$cvm, type = 'b')
  return(best_lamb)
}

tune_lamb2 <- function(alpha, x, y, k, seq){
  cv.out = cv.glmnet(x, y, alpha=alpha, lambda = seq, nfolds=k)
  bestlam = cv.out$lambda.min
  plot(cv.out$lambda, cv.out$cvm,type = 'b', pch=20)
  return(bestlam)
}

## Lasso Regression -------------------------------------
las_model = build_model(alpha=1, x, y)
las_lambda = find_best_lamb(alpha=1, model=las_model, x=x, y=y, k=10)
las_lambda
las_lambda2 = tune_lamb2(alpha=1, x=x, y=y, k=10, seq=seq(0,1,0.01))

## Ridge Regression ------------------------------------
rid_model = build_model(alpha=0, x, y)
rid_lambda = find_best_lamb(alpha=0, model=rid_model, x=x, y=y, k=10)
rid_lambda
rid_lambda2 = tune_lamb2(alpha=0, x=x, y=y, k=10, seq=seq(2,3,0.01))
rid_lambda2


## Source Code -------------------------------------------
# las_model$lambda[50]
# coef(las_model)[,50]
# sqrt(sum(coef(las_model)[-1,50]^2))
# 
# las_model$lambda[85]
# coef(las_model)[,85]
# sqrt(sum(coef(las_model)[-1,85]^2))
# 
# las_model$lambda[101]
# coef(las_model)[,101]
# sqrt(sum(coef(las_model)[-1,101]^2) )

# predict(las_model, s=3, type="coefficients")[1:18,]


# predict(lasso.mod, s=bestlam2, type="coefficients")[1:18,]
# predict(lasso.mod, s=50, type="coefficients")[1:18,]
# predict(lasso.mod, s=bestlam2, newx=x[1:10,])
# plot(cv.out2$lambda, cv.out2$cvm, type = 'b')


# cv.out=cv.glmnet(x,y,alpha=1,lambda = seq(0,1,0.01),nfolds=10)
# bestlam2 = cv.out$lambda.min
# cv.out$lambda
# cv.out$cvm
# predict(lasso.mod, s=bestlam2, type="coefficients")[1:18,]
# predict(lasso.mod, s=bestlam2, newx=x[1:10,])
# 
# plot(cv.out$lambda,cv.out$cvm,type = 'b', pch=20)






# Classification ------------------------------
## Prepare dataset ---------------------------
College2 = College
College2$Grad.Rate[College2$Grad.Rate>=70] = "High"
College2$Grad.Rate[College2$Grad.Rate<70] = "Low"
head(College2)
College2$Grad.Rate = factor(College2$Grad.Rate,levels=c("Low","High"),ordered =TRUE)
x = model.matrix(Grad.Rate~.,College2)[,-1]
y = College2$Grad.Rate


## Function ----------------------------------------------- 
build_model_logit <- function(alpha, x, y){
  grid =10^seq(10,-2, length=100)
  grid = c(grid,0)
  model = glmnet(x, y, alpha=alpha, family="binomial", lambda=grid)
  plot(model)
  return(model)
}

best_lamb_logit_mse <- function(alpha, x, y, k){
  cv.out = cv.glmnet(x, y, alpha=alpha, family="binomial", nfolds=k)
  best_lamb = cv.out$lambda.min
  plot(cv.out$lambda, cv.out$cvm, type = 'b')
  return(best_lamb)
}

tune_lamb2_logit <- function(alpha, x, y, k, seq){
  cv.out = cv.glmnet(x, y, alpha=alpha,family="binomial", lambda=seq, nfolds=k)
  bestlam = cv.out$lambda.min
  cv.out$lambda
  plot(cv.out$lambda, cv.out$cvm,type = 'b', pch=20)
  return(bestlam)
}

best_lamb_logit_acc <- function(alpha, x, y, k){
  folds = sample(1:k, length(y), replace=TRUE)
  grid = 10^(seq(-3,0.1, length.out=1000))
  cv.acc = matrix(rep(0,length(grid)*k), k, length(grid))
  
  for(j in 1:k){
    model = glmnet(x[folds!=j,], y[folds!=j], alpha=0, family="binomial", lambda=grid)
    pred = predict(model, s=grid, newx = x[folds==j,], type="class")
    cv.acc[j,]= colMeans(pred == y[folds==j])
  }
  
  acc = colMeans(cv.acc)
  which.max(colMeans(cv.acc))
  acc[which.max(colMeans(cv.acc))]
  bestlam = grid[which.max(colMeans(cv.acc))]
  
  plot(grid, acc*100, type='b')

  return(bestlam)
}
## Ridge Regression -------------------------
rid_model_logit = build_model_logit(alpha=0, x=x, y=y)

### Base on mse logit ----------------
rid_lamb_logit = best_lamb_logit_mse(alpha=0, x=x, y=y, k=10)
rid_lamb_logit
rid_lamb2_logit = tune_lamb2_logit(alpha=0, x=x, y=y, k=10, seq=seq(0,1,0.001))
rid_lamb2_logit

### Based on accuracy -----------------
rid_lamb_logit_acc = best_lamb_logit_acc(alpha=0, x=x, y=y, k=10)
predict(rid_model_logit, s=rid_lamb_logit_acc, type="coefficients")[1:18,]

## Lasso regression ----------------------------
las_model_logit = build_model_logit(alpha=1, x=x, y=y)
### Based on accuracy -----------------
las_lamb_logit_acc = best_lamb_logit_acc(alpha=1, x=x, y=y, k=10)
las_lamb_logit_acc
predict(las_model_logit, s=las_lamb_logit_acc, type="coefficients")[1:18,]



## Source Code ------------------------------
# grid=10^seq(10,-2, length=100)
# grid = c(grid,0)
# ridge.mod=glmnet(x,y,alpha=0,family="binomial",lambda=grid)

# cv.out3=cv.glmnet(x,y,alpha=0,family="binomial",nfolds=10)
# bestlam3 = cv.out3$lambda.min
# cv.out3$lambda
# cv.out3$cvm
# cv.out3$glmnet.fit
# predict(ridge.mod,s=bestlam3,type="coefficients")[1:18,]
# predict(ridge.mod,s=50,type="coefficients")[1:18,]
# predict(ridge.mod,s=bestlam3,newx=x[1:10,])

#tune lambda again
# cv.out3 = cv.glmnet(x,y,alpha=0,family="binomial", lambda = seq(0,0.03,0.001),nfolds=10)
# bestlam3 = cv.out3$lambda.min
# cv.out3$lambda
# predict(ridge.mod,s=bestlam3,type="coefficients")[1:18,]
# predict(ridge.mod,s=bestlam3,newx=x[1:10,],type="class")
# 
# cv.out3=cv.glmnet(x,y,alpha=0,family="binomial", lambda = seq(0,0.01,0.0005),nfolds=10)
# bestlam3 = cv.out3$lambda.min
# cv.out3$lambda


# k=10
# folds=sample(1:k,length(y),replace=TRUE)
# sum(folds==3)
# grid = 10^(seq(-3,0.1,length.out=1000))
# length(grid)
# cv.acc= matrix(rep(0,length(grid)*k),k,length(grid))
# 
# for(j in 1:k)
# {
#   ridge.mod2=glmnet(x[folds!=j,],y[folds!=j],alpha=0,family="binomial",lambda=grid)
#   ridge.pred = predict(ridge.mod2,s=grid,newx = x[folds==j,],type="class")
#   cv.acc[j,]= colMeans(ridge.pred == y[folds==j])
# }
# acc = colMeans(cv.acc)
# which.max(colMeans(cv.acc))
# acc[which.max(colMeans(cv.acc))]
# grid[which.max(colMeans(cv.acc))]
# 
# plot(grid,acc*100,type='b')
# 
# predict(ridge.mod,s=grid[which.max(colMeans(cv.acc))],type="coefficients")[1:18,]
# predict(ridge.mod,s=grid[which.max(colMeans(cv.acc))],type="coefficients")[1:18,]
