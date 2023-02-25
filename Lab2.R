# Set Environment/Library ----------------------
library(ISLR)
library(tictoc)
library(boot)


# Load and Explore Data --------------------
Auto <- Auto
head(Auto)
dim(Auto)
train=sample(dim(Auto)[1],floor(0.8*dim(Auto)[1]))
lm.fit= lm(mpg ~ horsepower, data=Auto, subset=train)
Auto[train,]

lm.fit
summary(lm.fit)
predict(lm.fit ,Auto[-train,])
attach(Auto)

mean((mpg[-train]-predict(lm.fit ,Auto[-train,]))^2)
cor(horsepower,mpg)
plot(horsepower,mpg)

x = 1:9


# Lab Section -----------------------------
# Using another feature to create model with different sampling method

# MY NOTE / COMMENT
# I choose same polynomial degree for horsepower and displacement in a model.
# The reason is that we can plot graph and see clearly how different results
#   from variant cross validation method

## Validation Set Method --------
sv.err = rep(NA,9)

# 1st sampling
for (i in 1:9){
  lm.fit=lm(mpg ~ poly(horsepower,i) + poly(displacement, i),data=Auto,subset=train)
  sv.err[i] = mean((mpg-predict(lm.fit,Auto))[-train]^2)
}
par(mfrow=c(1,1))
plot(x,sv.err,type = 'b',ylim=c(10,30), 
     xlab = "Polynomial Degree of Horsepower and Displacement", 
     ylab = "Error", 
     main = "Validation Set Method")

# another 4 sampling
whichmin = 1:5
whichmin[1] = which.min(sv.err)
for (j in 1:4){
  train=sample(dim(Auto)[1],floor(0.8*dim(Auto)[1]))
  
  for (i in 1:9){
    lm.fit=lm(mpg ~ poly(horsepower, i) + poly(displacement, i),data=Auto,subset=train)
    sv.err[i] = mean((mpg -predict(lm.fit,Auto))[-train]^2)
  }
  whichmin[j+1] = which.min(sv.err)
  lines(x, sv.err, col=j+1, pch=1, type='b')
}
whichmin

## LOOCV ----------------
# LOOCV on polynomial degree 1 to 9
tic("LOOCV")
cv.error=rep(0,9)
for (i in 1:9){
  glm.fit=glm(mpg ~ poly(horsepower, i) + poly(displacement, i), data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
toc()
cv.error
plot(1:9,cv.error,type='b', 
     xlab = "Polynomial Degree of Horsepower and Displacement",
     ylab = "Error",
     main = "LOOCV Method")

which.min(cv.error)
cv.error[which.min(cv.error)]


## Monte Carlo Method -------------
# Monte Carlo setting parameter
K=20
pp = 0.8
mmse = matrix(rep(0,9*K),K,9)

# 1st sampling
for (k in 1:K){   #loop for k times(like k-fold)
  train=sample(dim(Auto)[1],floor(pp*dim(Auto)[1]))
  
  for (i in 1:9){ #loop polynomial degree
    lm.fit=lm(mpg ~ poly(horsepower, i) + poly(displacement, i), data=Auto, subset=train)
    mmse[k,i] = mean((mpg -predict(lm.fit,Auto))[-train]^2)
  }
}
mtmse = colMeans(mmse)
which.min(mtmse)
plot(x,mtmse,type = 'b',ylim=c(10,30),
     xlab = "Polynomial Degree of Horsepower and Displacement", 
     ylab = "Error", 
     main = "Monte Carlo Method")

# another 4 sampling
for(j in 1:4){
  for (k in 1:K){   #loop for k times(like k-fold)
    train=sample(dim(Auto)[1],floor(pp*dim(Auto)[1]))
    
    for (i in 1:9){ #loop polynomial degree
      lm.fit=lm(mpg ~ poly(horsepower, i) + poly(displacement, i), data=Auto, subset=train)
      mmse[k,i] = mean((mpg -predict(lm.fit,Auto))[-train]^2)
    }
  }
  mtmse = colMeans(mmse)
  which.min(mtmse)
  lines(x, mtmse,type = 'b', col=j+1, pch=1)
}


## k-fold method -----------------------------------
#1st sampling
cv.error.10=rep(0,9)
tic("K-fold")
for (i in 1:9){
  glm.fit=glm(mpg~poly(horsepower,i) + poly(displacement,i), data=Auto)
  set.seed(10500000)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
toc()
cv.error.10
plot(1:9,cv.error.10,type='b',
     xlab = "Polynomial Degree of Horsepower and Displacement",
     ylab = "Error",
     main = "k-fold Method with k=10")

which.min(cv.error.10)
cv.error.10[which.min(cv.error.10)]


# another 4 sampling
wm = rep(0,5)
wm[1] = which.min(cv.error.10)
for (j in 1:4){
  for (i in 1:9){
    glm.fit=glm(mpg~poly(horsepower,i) + poly(displacement,i),data=Auto)
    set.seed(10*(j+6)^5)
    cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
  }
  lines(1:9, cv.error.10, col=j+1, pch=1, type='b')
  wm[j+1] = which.min(cv.error.10)
}
wm


# Example Code ------------------------------------
# Manual K-fold method from AJ that i not use

## Manual K-fold -----------------------

dim(Auto)[1]/10
k=10
folds=sample(1:k,dim(Auto)[1],replace=TRUE)
sum(folds == 4)             
cv.rmse= 1:k

for (i in 1:k){
  lm.fit= lm(mpg ~ poly(horsepower,7), data=Auto[folds!=i,])
  cv.rmse[i] = sqrt(mean((mpg[folds==i]-predict(lm.fit ,Auto[folds==i,]))^2))
}
cv.rmse
mean(cv.rmse)

## Manual K-fold, Restrict size ---------------------------

ss = floor(dim(Auto)[1]/10)

fold = sample(1:dim(Auto)[1],dim(Auto)[1],replace=FALSE)
mpg[fold[1:ss]]
cv.rmse= 1:k

for (i in 1:k){
  lm.fit= lm(mpg ~ poly(horsepower,7), data=Auto[-fold[(ss*(i-1)+1):(ss*i)],])
  cv.rmse[i] = sqrt(mean((mpg[fold[(ss*(i-1)+1):(ss*i)]]-predict(lm.fit ,Auto[fold[(ss*(i-1)+1):(ss*i)],]))^2))
}
cv.rmse
mean(cv.rmse)


















