# Lab Instruction ---------------------------------------
#Compare the following models
#1: Model from original data with your best threshold
#2: Model from SMOTE with your good choice of perc.over and perc.under


# Load and Explore Data --------------------------------------------------
library(ISLR)
library(xlsx)

data(Default)
head(Default)
names(Default)
dim(Default)
Default$student
sum(Default$default == "Yes")
sum(Default$default == "No")
summary(Default)
pairs(Default)
dd = cbind(Default$default=='Yes',Default$student=='Yes',Default[,3:4])
cor(dd)

# Binary logistic model -----------------------------------
glm.fits=glm(default~balance,data=Default,family=binomial)
summary(glm.fits)
coef(glm.fits)
nd = data.frame(seq(0,3000,0.01))
colnames(nd) = "balance"
nd[1:10,]
glm.probs=predict(glm.fits,newdata=nd,type="response")
plot(Default$balance,as.numeric(Default$default)-1)
lines(seq(0,3000,0.01),glm.probs, col=2)


# Multi binary logistic model --------------------------
## Fit model -------------------
glm.fits=glm(default~balance+student,data=Default,family=binomial)
summary(glm.fits)
coef(glm.fits)

## Set threshold and predict --------------
glm.probs=predict(glm.fits,type="response")
contrasts(Default$default)
glm.pred=rep("No",dim(Default)[1])
glm.pred[glm.probs >0.3]="Yes"
sum(glm.pred=="Yes")

## See result ----------------------------
table1 = table(glm.pred,Default$default) #confusion matrix
table1
mean(glm.pred==Default$default) #accuracy
fp = table1[2,1]/(table1[1,1]+table1[2,1]) #FP 
fn = table1[1,2]/(table1[1,2]+table1[2,2]) #FN
tp = table1[2,2]/(table1[1,2]+table1[2,2]) #TP
tn = table1[1,1]/(table1[1,1]+table1[2,1]) #TN
f1 = 2*tp/(2*tp+fp+fn) #F1

## Pseudo R-squared -------------------
library(pscl)
pR2(glm.fits)

## ROC Curve --------------------------
library(ROSE)
roc.curve(Default$default,glm.probs)



# LAB SECTION 1 SMOTE: Dealing with Unbalance Data ------------------------------- 
# Example oversampling and undersampling
# Oversampling of minor data: 333 * 7 + 333 = 2664
# Undersampling of major data: (333*7) * 1.15 = 2680

## See the unbalance data ------------------
sum(Default$default == "Yes")
sum(Default$default == "No")

## Synthesis data -----------------------
library(performanceEstimation)
znew <- smote(default~.,Default, perc.over=5, perc.under=1.2)
dim(znew)
sum(znew$default == "Yes")
sum(znew$default == "No")

## Fit model  --------------------------
glm.fits2=glm(default~balance+student,data=znew,family=binomial)
summary(glm.fits2)
coef(glm.fits2)

## Set threshold and predict --------------
glm.probs2=predict(glm.fits2,type="response")
glm.pred2=rep("No",dim(znew)[1])
glm.pred2[glm.probs2 >0.5]="Yes"

## See result ----------------------------
table2 = table(glm.pred2,znew$default) #confusion matrix
table2
mean(glm.pred2==znew$default) #Accuracy
fp_2 = table2[2,1]/(table2[1,1]+table2[2,1])#FP 
fn_2 = table2[1,2]/(table2[1,2]+table2[2,2])#FN 
tp_2 = table2[2,2]/(table2[1,2]+table2[2,2])#TP
tn_2 = table2[1,1]/(table2[1,1]+table2[2,1])#TN
f1_2 = 2*tp_2/(2*tp_2+fp_2+fn_2) #F1

## Test in original data ------------------
glm.probs2=predict(glm.fits2,newdata = Default,type="response")
glm.pred2=rep("No",dim(Default)[1])
glm.pred2[glm.probs2 >0.5]="Yes"
table3 = table(glm.pred2,Default$default)
table3
mean(glm.pred2==Default$default) #Accuracy
fp_3 = table3[2,1]/(table3[1,1]+table3[2,1]) #FP 
fn_3 = table3[1,2]/(table3[1,2]+table3[2,2]) #FN 
tp_3 = table3[2,2]/(table3[1,2]+table3[2,2]) #TP
tn_3 = table3[1,1]/(table3[1,1]+table3[2,1]) #TN
f1_3 = 2*tp_3/(2*tp_3+fp_3+fn_3) #F1

## Pseudo R-squared -------------------
pR2(glm.fits2)

## ROC Curve --------------------------
roc.curve(Default$default,glm.probs2)

## Discussion Result ------------------
# I use perc.over = 5 and perc.under = 1.2, and data are balance,
#   no.data of "Yes" = "No" = 1998.
#   Moreover, I use threshold equal 0.5 because data are already balance.
#   The result is also satisfied from testing with original data.
#   F1-score is approximately 0.88, and TP are 296.
#   Although FP are much (1244), but FN aren't much (37).
#   In this case (default prediction) and general case, FN is more critical concern. 



# Discriminant line: Dealing with Unbalance Data --------------
## Fit model ------------------------------
glm.fits=glm(default~balance,data=Default,family=binomial)
summary(glm.fits)
coef(glm.fits)

## Plot default vs balance ---------------
nd = data.frame(seq(0,3000,0.01))
colnames(nd) = "balance"
nd[1:10,]
glm.probs=predict(glm.fits,newdata=nd,type="response")
plot(Default$balance,as.numeric(Default$default)-1)
lines(seq(0,3000,0.01),glm.probs, col=2)

## Draw a discriminant line ---------------
coef(glm.fits)
pp = 0.5
#log(pp/(1-pp)) = B0 + B1x
abline(v=(log(pp/(1-pp))-coef(glm.fits)[1])/coef(glm.fits)[2],col = 'blue')
pp = 0.2
abline(v=(log(pp/(1-pp))-coef(glm.fits)[1])/coef(glm.fits)[2],col = 'green')


# Multi binary logistic model plot discriminant line ---------------------
glm.fits=glm(default~balance+income,data=Default,family=binomial)
summary(glm.fits)
coef(glm.fits)

plot(Default$balance,Default$income,col= as.numeric(Default$default))
#log(pp/(1-pp)) = B0 + B1x1 + B2x2
#x2 = (log(pp/(1-pp))-B0)/B2-(B1/B2)x1
pp=0.5
abline(a = (log(pp/(1-pp))-coef(glm.fits)[1])/coef(glm.fits)[3],b=-coef(glm.fits)[2]/coef(glm.fits)[3],col='blue',lwd=2)
pp=0.2
abline(a = (log(pp/(1-pp))-coef(glm.fits)[1])/coef(glm.fits)[3],b=-coef(glm.fits)[2]/coef(glm.fits)[3],col='green',lwd=2)
pp=0.1
abline(a = (log(pp/(1-pp))-coef(glm.fits)[1])/coef(glm.fits)[3],b=-coef(glm.fits)[2]/coef(glm.fits)[3],col='purple',lwd=2)




#  LAB SECTION 2 k-fold Cross Validation with original data ---------------------------
## k-fold setting ---------------------------
k=10
folds=sample(1:k,dim(Default)[1],replace=TRUE)
sum(folds == 1)             
cv.acc= 1:k
tp = 1:k
tn = 1:k
fp = 1:k
fn = 1:k
f1 = 1:k

## Loop for iteration ---------------------
for(j in 1:k){
  glm.fits3=glm(default~balance+student,data=Default,subset= (folds != j),family=binomial)
  glm.probs3=predict(glm.fits3,newdata = Default[folds == j,],type="response")
  glm.pred3=rep("No",sum(folds == j))
  glm.pred3[glm.probs3 >0.3]="Yes" #Threshold 0.3
  table1 = table(glm.pred3,Default$default[folds == j])
  cv.acc[j]= mean(glm.pred3==Default$default[folds == j])
  fp = table1[2,1]/(table1[1,1]+table1[2,1])
  fn = table1[1,2]/(table1[1,2]+table1[2,2])
  tp = table1[2,2]/(table1[1,2]+table1[2,2])
  tn = table1[1,1]/(table1[1,1]+table1[2,1])
}

## Calculate average score ----------------------
cv.acc
mean(cv.acc)
cv.tn = mean(tn)
cv.tp = mean(tp)
cv.fn = mean(fn)
cv.fp = mean(fp)
cv.f1 = 2*cv.tp/(2*cv.tp+cv.fp+cv.fn)
cv.tp
cv.tn
cv.f1

## Discussion Result ---------------------------
# I choose threshold at 0.3. The reason is that
#   unbalance dataset that most data is negative, 
#   and model will predict negative more than positive.
#   To handle this with simple method, the threshold should less than 0.5 for 
#   predicting more positive result. However, it must not too low for predicting
#   almost data to positive.


