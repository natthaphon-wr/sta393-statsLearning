# Set Environment --------------------------------------------------------------
library(tree)
library(MASS)
library(randomForest)
library(caret)

# Data Preparation -------------------------------------------------------------
college=read.csv("College.csv",header=T,na.strings="?")
rownames(college) = college[,1]
college=data.frame(college[,-1])
college$Private = as.factor(college$Private)
head(college)
dim(college)
sum(is.na(college))
train = sample(1:nrow(college), nrow(college)/2) #train/test: 50/50


# Regression Trees -------------------------------------------------------------

## Basic Tree -------------------------------------------
tree.college = tree(Grad.Rate~.,college ,subset=train) 
summary(tree.college)
plot(tree.college)
text(tree.college ,pretty=0)

### Pruning -------------------------------
cv.college=cv.tree(tree.college)
cv.college$size[which.min(cv.college$dev)]
plot(cv.college$size ,cv.college$dev ,type='b')
cv.college$size[which.min(cv.college$dev)]
prune.college=prune.tree(tree.college ,best=cv.college$size[which.min(cv.college$dev)])
plot(prune.college)
text(prune.college ,pretty=0)

### Predict with test-set ------------------
yhat=predict(prune.college ,newdata=college[-train ,])
college.test=college[-train ,"Grad.Rate"]
plot(yhat ,college.test)
abline(0,1)
mean((yhat-college.test)^2)
sqrt(mean((yhat-college.test)^2)) #RMSE

par(mfrow=c(1,1))


## Bagging ------------------------------------------------------
# mtry = all
bag.college=randomForest(Grad.Rate~.,data=college ,subset=train, mtry=ncol(college)-1, ntree = 100,importance =TRUE)
bag.college

yhat.bag = predict(bag.college ,newdata=college[-train ,])
plot(yhat.bag, college.test)
abline(0,1)
sqrt(mean((yhat.bag-college.test)^2)) #RMSE

## Random Forest -----------------------------------------------
# mtry = 4
rf.college=randomForest(Grad.Rate~.,data=college ,subset=train ,mtry=4,ntree=100)

yhat.rf = predict(rf.college,newdata=college[-train,])
plot(yhat.rf, college.test)
abline(0,1)
sqrt(mean((yhat.rf-college.test)^2)) #RMSE

### Feature Importance -------------------
rf.college=randomForest(Grad.Rate~.,data=college ,mtry=4,ntree=100,importance =TRUE) #5
importance (rf.college)
varImp(rf.college)
varImpPlot(rf.college,type=2)
varImpPlot(rf.college ,type=1)
dim(college)


# Regression with Cross Validation --------------------------------------------

## Pruning Basic Tree -------------------------------------
k=10
folds=sample(1:k,nrow(college),replace=TRUE)
cv.mse.prune= 1:k
for(j in 1:k)
{
  tree.college=tree(Grad.Rate~.,college[folds!=j,])
  cv.college=cv.tree(tree.college)
  prune.college=prune.tree(tree.college ,best=cv.college$size[which.min(cv.college$dev)])
  cv.mse.prune[j] = mean((college$Grad.Rate[folds==j]-predict(prune.college ,newdata=college[folds==j,]))^2)
}
sqrt(mean(cv.mse.prune)) #RMSE

## Random Forest ----------------------------------------------
cv.mse.rf = 1:16
for (m in 2:17){ #loop for feature selection
  cvv = 1:k
  for(j in 1:k)
  {
    rf.college = randomForest(Grad.Rate~.,data=college[folds!=j,], mtry=m,ntree=100,importance =TRUE)
    cvv[j] = mean((college$Grad.Rate[folds==j]-predict(rf.college ,newdata=college[folds==j,]))^2)
  }
  cv.mse.rf[m-1] = sqrt(mean(cvv))
}
cv.mse.rf
which.min(cv.mse.rf)+1
min(cv.mse.rf)


# Classification Trees ---------------------------------------------------------
## Preparation ----------------------------------------------
college2 = college
college2$Grad.Rate[college2$Grad.Rate>=70] = "High"
college2$Grad.Rate[college2$Grad.Rate<70] = "Low"
head(college2)
college2$Grad.Rate = factor(college2$Grad.Rate,levels=c("Low","High"),ordered =TRUE)

#Set Validation 
dim(college2)
train=sample(1:nrow(college2),floor(nrow(college2)*0.8)) #Train/Test: 80/20
college2.test=college2[-train,]
length(train)
Grad.Rate.test=college2$Grad.Rate[-train]

## Basic Tree ----------------------------------------------
# tree.college=tree(Grad.Rate~.,college2)
# summary(tree.college)
# plot(tree.college)
# text(tree.college,pretty=0)

tree.college =tree(Grad.Rate~. ,college2 ,subset=train)
summary(tree.college)
plot(tree.college)
text(tree.college,pretty=0)

### Predict with test-set -----------------------------
#Choose cutoff
tree.prob = predict(tree.college,college2.test)
tree.pred = rep('Low', nrow(tree.prob))
tree.pred[tree.prob[,2]>0.7] = 'High'
tree.pred = factor(tree.pred,levels=c("Low","High"),ordered =TRUE)

#default "mode"
# tree.pred = predict(tree.college,college2.test,type="class")

table1 = table(tree.pred,Grad.Rate.test)
acc = (table1[1,1]+table1[2,2])/sum(table1)
fp = table1[2,1]/(table1[1,1]+table1[2,1])
fn = table1[1,2]/(table1[1,2]+table1[2,2])
tp = table1[2,2]/(table1[1,2]+table1[2,2])
tn = table1[1,1]/(table1[1,1]+table1[2,1])
f1 = 2*tp/(2*tp+fp+fn)
acc
tp
tn
f1

#Cross Validation (Not valid)
# cv.college=cv.tree(tree.college,FUN=prune.misclass)
# names(cv.college)
# cv.college
# par(mfrow=c(1,2))
# plot(cv.college$size ,cv.college$dev ,type="b")
# plot(cv.college$k ,cv.college$dev ,type="b")
# which.min(cv.college$dev)
# cv.college$size[which.min(cv.college$dev)]
# 
# dim(college2)


#best pruning (Not valid)
# prune.college=prune.misclass(tree.college ,best=cv.college$size[which.min(cv.college$dev)])
# par(mfrow=c(1,1))
# plot(prune.college)
# text(prune.college ,pretty=0)
# tree.pred=predict(prune.college,college2.test,type="class")
# table1 = table(tree.pred,Grad.Rate.test)
# acc=(table1[1,1]+table1[2,2])/sum(table1)
# acc
# fp = table1[2,1]/(table1[1,1]+table1[2,1])
# #FN 
# fn = table1[1,2]/(table1[1,2]+table1[2,2])
# #TP
# tp = table1[2,2]/(table1[1,2]+table1[2,2])
# #TN
# tn = table1[1,1]/(table1[1,1]+table1[2,1])
# #F1
# f1 = 2*tp/(2*tp+fp+fn)
# 
# tp
# tn
# f1

## Prune with Specify Size of Tree -----------------------------------
#size = 4
prune.college=prune.misclass(tree.college,best=4)
plot(prune.college)
text(prune.college ,pretty=0)
tree.pred=predict(prune.college,college2.test,type="class")
table1 = table(tree.pred,Grad.Rate.test)
acc=(table1[1,1]+table1[2,2])/sum(table1)
fp = table1[2,1]/(table1[1,1]+table1[2,1])
fn = table1[1,2]/(table1[1,2]+table1[2,2])
tp = table1[2,2]/(table1[1,2]+table1[2,2])
tn = table1[1,1]/(table1[1,1]+table1[2,1])
f1 = 2*tp/(2*tp+fp+fn)
acc
tp
tn
f1

## Random Forest -------------------------------------------
rf.college = randomForest(Grad.Rate~., data=college2 ,subset=train ,mtry=4, ntree = 100)
rf.pred=predict(rf.college,college2.test,type="class")

table1 = table(rf.pred,Grad.Rate.test)
acc=(table1[1,1]+table1[2,2])/sum(table1)
fp = table1[2,1]/(table1[1,1]+table1[2,1])
fn = table1[1,2]/(table1[1,2]+table1[2,2])
tp = table1[2,2]/(table1[1,2]+table1[2,2])
tn = table1[1,1]/(table1[1,1]+table1[2,1])
f1 = 2*tp/(2*tp+fp+fn)
acc
tp
tn
f1

### Feature Importance ----------------
rf.college = randomForest(Grad.Rate~.,data=college2 ,mtry=4, ntree = 100,importance =TRUE)
imp1 = importance(rf.college)
varImp(rf.college)
varImpPlot(rf.college ,type=2)
varImpPlot(rf.college ,type=1)


# Classification with k-fold ---------------------------------------------------

## Random Forest ------------------------------------------
k=10
folds=sample(1:k,nrow(college2),replace=TRUE)
cv.acc.rf = 1:15
for (m in 2:16){
  cvv = 1:k
  for(j in 1:k)
  {
    rf.college = randomForest(Grad.Rate~.,data=college2[folds!=j,], mtry=m,ntree=100,importance =TRUE)
    rf.pred=predict(rf.college,college2[folds==j,],type="class")
    cvv[j] = mean(as.numeric(rf.pred) == as.numeric(college2$Grad.Rate[folds==j]))
  }
  cv.acc.rf[m-1] = sqrt(mean(cvv))
}
cv.acc.rf
npred_rf_k <- which.max(cv.acc.rf)+1
acc_rf_k <- max(cv.acc.rf)
npred_rf_k
acc_rf_k

#LAB: 
# 1. Using k-fold for pruning -> check acc
# 2. RF change cut-off -> check acc

# LAB SECTION: Classification Trees --------------------------------------------

## k-fold pruning ------------------------------------------
k=10
folds=sample(1:k,nrow(college),replace=TRUE)
cv.acc.prune= 1:k
for(j in 1:k){
  tree.college = tree(Grad.Rate~., college2[folds!=j,])
  cv.college = cv.tree(tree.college)
  prune.college = prune.tree(tree.college, best=cv.college$size[which.min(cv.college$dev)])
  prune.pred = predict(prune.college, college2[folds==j,],type="class")
  cvv = mean(as.numeric(prune.pred) == as.numeric(college2$Grad.Rate[folds==j]))
  cv.acc.prune[j] = sqrt(mean(cvv))
}
acc_pru_k <- max(cv.acc.prune)
acc_pru_k

## Random Forrest with different cutoff ------------------
k=10
folds=sample(1:k,nrow(college2),replace=TRUE)
cv.acc.rf = 1:15
for (m in 2:16){
  cvv = 1:k
  for(j in 1:k){
    rf.college = randomForest(Grad.Rate~.,data=college2[folds!=j,], mtry=m, ntree=100, importance =TRUE)
    rf.prob = predict(rf.college, college2[folds==j,], type="prob")
    rf.pred = rep('Low', nrow(rf.prob))
    rf.pred[rf.prob[,2]>0.7] = 'High'
    rf.pred = factor(rf.pred, levels=c("Low","High"), ordered =TRUE)
    cvv[j] = mean(as.numeric(rf.pred) == as.numeric(college2$Grad.Rate[folds==j]))
  }
  cv.acc.rf[m-1] = sqrt(mean(cvv))
}
cv.acc.rf
npred_rfcut_k <- which.max(cv.acc.rf)+1 #no. of predictors
acc_rfcut_k <- max(cv.acc.rf)
npred_rfcut_k
acc_rfcut_k



