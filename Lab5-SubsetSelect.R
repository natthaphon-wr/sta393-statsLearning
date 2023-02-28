# Set Library --------------------------------
library(leaps)
library(tictoc)
library(boot)
library(bestglm)


# Import Data ------------------------------
college=read.csv("College.csv",header=T,na.strings="?")
rownames(college) = college[,1]
college=college[,-1]
head(college)
dim(college)
cor(college[sapply(college, is.numeric)])


# Predict Function -------------------------------
predict.regsubsets = function(object ,newdata ,id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}


# Best Subset Selection ------------------------------
## Define selection method ------------------
regfit.full = regsubsets(Grad.Rate~.,college)
summary(regfit.full)

regfit.full = regsubsets(Grad.Rate~.,college,nvmax=17)
reg.summary = summary(regfit.full)

## Indirect Metrics ------------------------
names(reg.summary)
reg.summary$which[5,]
reg.summary$bic
reg.summary$rsq
reg.summary$adjr2
reg.summary$cp

#RSS
par(mfrow=c(1,1))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS", type="b")
points(which.min(reg.summary$rss), reg.summary$rss[which.min(reg.summary$rss)], col="red",cex=2,pch=20)

#Adjusted r-squared
plot(reg.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq", type="b")
which.max(reg.summary$adjr2)
points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)], col="red",cex=2,pch=20)

#Cps
plot(reg.summary$cp ,xlab="Number of Variables ",ylab="Cp", type="b")
which.min(reg.summary$cp)
points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red",cex=2,pch=20)

#BIC
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC", type="b")
which.min(reg.summary$bic)
points(which.min(reg.summary$bic),reg.summary$bi[which.min(reg.summary$bic)],col="red",cex=2,pch=20)

### Indirect Method Summary ---------------------
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

### Predict -----------------------------
coef(regfit.full,which.min(reg.summary$bic))
predict(regfit.full, newdata=college, id=which.min(reg.summary$bic))
dim(college)


## Direct Metrics (CV) -------------------------------
### Get optimal feature -------------------
cv.error.10 = 1:17
for (i in 1:17){
  glm.fit=glm(Grad.Rate~.,data=college[,c(reg.summary$which[i,-1],TRUE)])
  set.seed(500)
  cv.error.10[i]=cv.glm(college[,c(reg.summary$which[i,-1],TRUE)],glm.fit,K=10)$delta[1]
}
cv.error.10
which.min(cv.error.10)
plot(1:17,cv.error.10,type='b')
points(which.min(cv.error.10),cv.error.10[which.min(cv.error.10)], col="red",cex=2,pch=20)

### Fit model -------------------
final.fit.cv = glm(Grad.Rate~.,data=college[,c(reg.summary$which[which.min(cv.error.10),-1],TRUE)])
summary(final.fit.cv)

final.fit.cp =glm(Grad.Rate~.,data=college[,c(reg.summary$which[which.min(reg.summary$cp),-1],TRUE)])
summary(final.fit.cp)

final.fit.bic =glm(Grad.Rate~.,data=college[,c(reg.summary$which[which.min(reg.summary$bic),-1],TRUE)])
summary(final.fit.bic)


# Forward, Backward, Hybrid Selection -----------------------------
## Define selection method ------------------
regfit.fwd = regsubsets (Grad.Rate~.,college, nvmax=17, method="forward")
regfit.bwd = regsubsets (Grad.Rate~.,college, nvmax=17, method="backward")
regfit.hyb = regsubsets (Grad.Rate~.,college, nvmax=17, method="seqrep")
reg.summary.fwd = summary(regfit.fwd)
reg.summary.bwd = summary(regfit.bwd)
reg.summary.hyb = summary(regfit.hyb)

coef(regfit.full,10)
coef(regfit.fwd,10)
coef(regfit.bwd,10)
coef(regfit.hyb,10)

## Indirect Metrics ------------------------
#Cp
which.min(reg.summary.fwd$cp)
which.min(reg.summary.bwd$cp)
which.min(reg.summary.hyb$cp)
coef(regfit.full,which.min(reg.summary$cp))
coef(regfit.fwd,which.min(reg.summary.fwd$cp))
coef(regfit.bwd,which.min(reg.summary.bwd$cp))
coef(regfit.hyb,which.min(reg.summary.hyb$cp))

#BIC
which.min(reg.summary.fwd$bic)
which.min(reg.summary.bwd$bic)
which.min(reg.summary.hyb$bic)
coef(regfit.full,which.min(reg.summary$bic))
coef(regfit.fwd,which.min(reg.summary.fwd$bic))
coef(regfit.bwd,which.min(reg.summary.bwd$bic))
coef(regfit.hyb,which.min(reg.summary.hyb$bic))

#Adjusted r-squared
which.max(reg.summary.fwd$adjr2)
which.max(reg.summary.bwd$adjr2)
which.max(reg.summary.hyb$adjr2)
coef(regfit.full,which.max(reg.summary$adjr2))
coef(regfit.fwd,which.max(reg.summary.fwd$adjr2))
coef(regfit.bwd,which.max(reg.summary.bwd$adjr2))
coef(regfit.hyb,which.max(reg.summary.hyb$adjr2))

### Predict -------------------------
predict(regfit.full, college, id=7)

## Direct Metrics (CV) ----------------------------------
### Forward Method -------------------
cv.error.10.fwd=1:17
for (i in 1:17){
  glm.fit=glm(Grad.Rate~.,data=college[,c(reg.summary.fwd$which[i,-1],TRUE)])
  set.seed(1)
  cv.error.10.fwd[i]=cv.glm(college[,c(reg.summary.fwd$which[i,-1],TRUE)],glm.fit,K=10)$delta[1]
}
cv.error.10.fwd
plot(1:17, cv.error.10.fwd, type='b', main="Stepwise Forward Selction")
points(which.min(cv.error.10.fwd),cv.error.10.fwd[which.min(cv.error.10.fwd)], col="red",cex=2,pch=20)

final.fit.fwd =glm(Grad.Rate~.,data=college[,c(reg.summary.fwd$which[which.min(cv.error.10.fwd),-1],TRUE)])
summary(final.fit.fwd)

### Backward Method ---------------------
cv.error.10.bwd=1:17
for (i in 1:17){
  glm.fit = glm(Grad.Rate~., data=college[,c(reg.summary.bwd$which[i,-1], TRUE)])
  set.seed(1)
  cv.error.10.bwd[i] = cv.glm(college[,c(reg.summary.bwd$which[i,-1],TRUE)], glm.fit,K=10)$delta[1]
}
cv.error.10.bwd
plot(1:17, cv.error.10.bwd, type='b', main="Stepwise Backward Selction")
points(which.min(cv.error.10.bwd), cv.error.10.bwd[which.min(cv.error.10.bwd)], col="red",cex=2,pch=20)

final.fit.bwd = glm(Grad.Rate~., data=college[,c(reg.summary.bwd$which[which.min(cv.error.10.bwd),-1],TRUE)])
summary(final.fit.bwd)

### Hybrid Method ---------------------
cv.error.10.hyb=1:17
for (i in 1:17){
  glm.fit = glm(Grad.Rate~., data=college[,c(reg.summary.hyb$which[i,-1], TRUE)])
  set.seed(1)
  cv.error.10.hyb[i] = cv.glm(college[,c(reg.summary.hyb$which[i,-1],TRUE)], glm.fit,K=10)$delta[1]
}
cv.error.10.hyb
plot(1:17, cv.error.10.hyb, type='b', main="Stepwise Hybrid Selction")
points(which.min(cv.error.10.hyb), cv.error.10.hyb[which.min(cv.error.10.hyb)], col="red",cex=2,pch=20)

final.fit.hyb = glm(Grad.Rate~., data=college[,c(reg.summary.hyb$which[which.min(cv.error.10.hyb),-1],TRUE)])
summary(final.fit.hyb)



# Classification ---------------------------------------
### Modify and create new dataset ---------
college2 = college[,-c(5,10,11)]
college2$Grad.Rate[college2$Grad.Rate>=70] = "High"
college2$Grad.Rate[college2$Grad.Rate<70] = "Low"
head(college2)
college2$Grad.Rate = factor(college2$Grad.Rate,levels=c("Low","High"),ordered =TRUE)
college2$Private = as.factor(college2$Private)
colnames(college2)[length(college2)] = "y"
summary(college2)

## Stepwise using AIC ----------------------
### Fit model -----------------
hyb_model_log = bestglm(Xy = college2, family = binomial, IC = "AIC", method = "seqrep")
dim(college2)
hyb_model_log$BestModel
hyb_model_log$BestModels
hyb_model_log$Subsets
hyb_model_log$ModelReport
coef(hyb_model_log$BestModel)

### Predict -------------------
predict(hyb_model_log$BestModel,newdata = college2, type='response')


## Stepwise using BIC ----------------------
### Fit model -----------------
hyb_model_log_bic = bestglm(Xy = college2, family = binomial, IC = "BIC", method = "seqrep")
hyb_model_log_bic$BestModel
hyb_model_log_bic$BestModels
hyb_model_log_bic$Subsets
hyb_model_log_bic$ModelReport
coef(hyb_model_log_bic$BestModel)

### Predict -------------------
predict(hyb_model_log_bic$BestModel, newdata = college2, type='response')




