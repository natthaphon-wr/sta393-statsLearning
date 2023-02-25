# WVS Data set ---------------------------------------------------
# It's ordinal data.

## Load and explore data ----------------------------
library(carData)
library(MASS)
data(WVS) 
head(WVS)
dim(WVS)
sum(WVS$poverty == "Too Much")
sum(WVS$poverty == "About Right")
summary(WVS)

## Logistic Regression -----------------------
full.model <- polr(poverty~religion+degree+country+age+gender, data = WVS, Hess = TRUE)
summary(full.model)
ctable = coef(summary(full.model))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable = cbind(ctable, "p value" = p)

pred = predict(full.model,WVS)
prob = predict(full.model,WVS,type = "probs")

# tune classification from probability
pred2 = rep("Too Little",length(pred))
pred2[prob[,2]+0.1>prob[,1]] = "About Right"
pred2[prob[,3]>0.25] = "Too Much" 
pred2 = factor(pred2,levels=c("Too Little","About Right","Too Much"),ordered =TRUE)

table(WVS$poverty, pred)
table(WVS$poverty, pred2)
mean(as.character(WVS$poverty)==as.character(pred))
mean(as.character(WVS$poverty)==as.character(pred2))

## k-folds with logistic regression -----------------------------
k=10
folds=sample(1:k,dim(WVS)[1],replace=TRUE)
sum(folds == 10)             
cv.acc= 1:k
c1.acc=1:k
c2.acc=1:k
c3.acc=1:k

for(j in 1:k){
  full.model <- polr(poverty~religion+degree+country+age+gender, 
                     data = WVS, subset=(folds!=j), Hess=TRUE)
  prob = predict(full.model, WVS[folds==j,], type="probs")
  
  pred2 = rep("Too Little",sum(folds == j))
  pred2[prob[,2]+0.1>prob[,1]]="About Right"
  pred2[prob[,3]>0.25] = "Too Much" 
  pred2 = factor(pred2,levels=c("Too Little","About Right","Too Much"),ordered =TRUE)
  
  table3 = table(WVS$poverty[folds == j], pred2)
  cv.acc[j]= mean(pred2==WVS$poverty[folds == j])
  c1.acc[j] = table3[1,1]/(table3[1,1]+table3[1,2]+table3[1,3])
  c2.acc[j] = table3[2,2]/(table3[2,1]+table3[2,2]+table3[2,3])
  c3.acc[j] = table3[3,3]/(table3[3,1]+table3[3,2]+table3[3,3])
}

cv.acc
mean(cv.acc)
mean(c1.acc)
mean(c2.acc)
mean(c3.acc)


## LAB: KNN model -------------------------------
#Convert to numerical variables
WVS[, c('poverty', 'religion', 'degree', 'country', 'gender')] <- 
  sapply(WVS[, c('poverty', 'religion', 'degree', 'country', 'gender')], unclass)

#split data with simple method
set.seed(14)
sample <- sample(c(TRUE, FALSE), nrow(WVS), replace=TRUE, prob=c(0.8,0.2))
train_wvs  <- WVS[sample, ]
test_wvs   <- WVS[!sample, ]

#knn model
library(class)
acc_list = list()

for (i in 1:30){
  knn.pred = knn(train_wvs,
               test_wvs,
               train_wvs$poverty,
               k=i)
  table_pred = table(knn.pred, test_wvs$poverty)
  acc = mean(knn.pred == test_wvs$poverty)
  acc_list <- append(acc_list, acc)
    
}
plot(1:30, acc_list,
     main = "Total Accuracy of KNN Model",
     xlab = "K",
     ylab = "Accuracy")

# From the graph, it's showed reasonable result that accuracy will decrease 
#   while k is increasing. However, we will not use models that k are too small
#   because of overfitting. I think we can use model with k=5 or k=10 with accuracy 
#   are above 0.94. Moreover, there is a big concern about this KNN model. 
#   The reason is "How good conversion method from categorical to numerical" 
#   (order?, distance?).


# cmcData Data set -------------------------------------------------------
# It's nominal. http://r-statistics.co/Multinomial-Regression-With-R.html
# 1=No-use, 2=Long-term, 3=Short-term

## Load and explore data -----------------------
cmcData <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data", stringsAsFactors=FALSE, header=F)
colnames(cmcData) <- c("wife_age", "wife_edu", "hus_edu", "num_child", "wife_rel", "wife_work", "hus_occu", "sil", "media_exp", "cmc")
head(cmcData)
?multinom

sum(cmcData$cmc==1)
sum(cmcData$cmc==2)
sum(cmcData$cmc==3)

summary(cmcData)

## Multinomial log-linear models ----------------------------
library(nnet)
multinom.cmc<- multinom(cmc~., data=cmcData)
summary(multinom.cmc)

predict.cmc <- predict(multinom.cmc, cmcData, "probs")
predict.cmc.class <- predict(multinom.cmc, cmcData)
table(cmcData$cmc,predict.cmc.class)
mean(as.character(predict.cmc.class) == as.character(cmcData$cmc))

## k-folds CV ---------------------------------
# Compare multinomial log-linear and KNN
library(class)

k=10
folds=sample(1:k,dim(cmcData)[1],replace=TRUE)
sum(folds == 1)             
cv.acc= matrix(rep(0,2*k),k,2)
c1.acc= matrix(rep(0,2*k),k,2)
c2.acc= matrix(rep(0,2*k),k,2)
c3.acc= matrix(rep(0,2*k),k,2)

for(j in 1:k){
  #multinomial
  multinom.cmc <- multinom(cmc~., data=cmcData, subset=(folds != j))
  predict.cmc.class <- predict(multinom.cmc, cmcData[folds == j,])
  table.multi = table(predict.cmc.class, cmcData$cmc[folds == j])
  
  #knn
  knn.pred = knn(cmcData[folds != j,-ncol(cmcData)], 
                 cmcData[folds == j,-ncol(cmcData)], 
                 cmcData$cmc[folds != j], 
                 k=10)
  table.knn = table(knn.pred,cmcData$cmc[folds == j])
  
  #accuracy from muilti
  cv.acc[j,1] = mean(predict.cmc.class==cmcData$cmc[folds == j])
  c1.acc[j,1] = table.multi[1,1]/(table.multi[1,1]+table.multi[1,2]+table.multi[1,3])
  c2.acc[j,1] = table.multi[2,2]/(table.multi[2,1]+table.multi[2,2]+table.multi[2,3])
  c3.acc[j,1] = table.multi[3,3]/(table.multi[3,1]+table.multi[3,2]+table.multi[3,3])
  
  #accuracy from knn
  cv.acc[j,2] = mean(knn.pred==cmcData$cmc[folds == j])
  c1.acc[j,2] = table.knn[1,1]/(table.knn[1,1]+table.knn[1,2]+table.knn[1,3])
  c2.acc[j,2] = table.knn[2,2]/(table.knn[2,1]+table.knn[2,2]+table.knn[2,3])
  c3.acc[j,2] = table.knn[3,3]/(table.knn[3,1]+table.knn[3,2]+table.knn[3,3])
}

cv.acc
colMeans(cv.acc)
colMeans(c1.acc)
colMeans(c2.acc)
colMeans(c3.acc)

## LAB: Tune KNN ------------------------------------
library(class)

#split data with simple method
set.seed(14)
sample <- sample(c(TRUE, FALSE), nrow(cmcData), replace=TRUE, prob=c(0.8,0.2))
train_cmc  <- cmcData[sample, ]
test_cmc   <- cmcData[!sample, ]

acc_list = list()
acc1_list = list()
acc2_list = list()
acc3_list = list()
for (i in 1:30){
  knn.pred = knn(train_cmc, 
                 test_cmc, 
                 train_cmc$cmc, 
                 k=i)
  table_pred = table(knn.pred, test_cmc$cmc)
  acc = mean(knn.pred == test_cmc$cmc)
  acc1 = table_pred[1,1]/(table_pred[1,1]+table_pred[1,2]+table_pred[1,3])
  acc2 = table_pred[2,2]/(table_pred[2,1]+table_pred[2,2]+table_pred[2,3])
  acc3 = table_pred[3,3]/(table_pred[3,1]+table_pred[3,2]+table_pred[3,3])
  acc_list <- append(acc_list, acc)
  acc1_list <- append(acc1_list, acc1)
  acc2_list <- append(acc2_list, acc2)
  acc3_list <- append(acc3_list, acc3)
}

plot(1:30, acc_list,
     main = "Total Accuracy of KNN Model",
     xlab = "K",
     ylab = "Accuracy")
which.max(acc_list)
max(unlist(acc_list))

plot(1:30, acc1_list,
     main = "No-use Accuracy of KNN Model",
     xlab = "K",
     ylab = "Accuracy")
which.max(acc1_list)
max(unlist(acc1_list))

plot(1:30, acc2_list,
     main = "Long-term Accuracy of KNN Model",
     xlab = "K",
     ylab = "Accuracy")
which.max(acc2_list)
max(unlist(acc2_list))

plot(1:30, acc3_list,
     main = "Short-term Accuracy of KNN Model",
     xlab = "K",
     ylab = "Accuracy")
which.max(acc3_list)
max(unlist(acc3_list))

# From those results and graphs, it's showed that optimized KNN model is 
#   the model with K=4, according total accuracy of 3 classes.



# Generated Dataset ---------------------------------------------
## correlated sample -----------------------
library(colorednoise)

#class 1
mus <- c(0, 3)
sd <- c(2, 1)
cr <- matrix(c(1, 0.5, 0.5, 1), ncol = 2)
sigmas = diag(sd)%*%cr%*%diag(sd)
class1 <- multi_rnorm(200, mus, sigmas)
colMeans(class1)
cor(class1)

#class 2
mus <- c(-1, 2)
sd <- c(1, 1)
cr <- matrix(c(1, 0.2, 0.2, 1), ncol = 2)
sigmas = diag(sd)%*%cr%*%diag(sd)
class2 <- multi_rnorm(200, mus, sigmas)
colMeans(class2)
cor(class2)

#class 3
mus <- c(2, -2)
sd <- c(1, 2)
cr <- matrix(c(1, 0.3, 0.3, 1), ncol = 2)
sigmas = diag(sd)%*%cr%*%diag(sd)
class3 <- multi_rnorm(200, mus, sigmas)
colMeans(class3)
cor(class3)

#combine data
data = rbind(class1,class2,class3)
data = cbind(data,c(rep(1,200),rep(2,200),rep(3,200)))
colnames(data) = c("var1","var2","class")
data = data.frame(data)
head(data)
plot(data[,1:2],col=data[,3])

# #correlated sample2
# mus <- c(0, 3)
# sd <- c(2, 1)
# cr <- matrix(c(1, 0.5, 0.5, 1), ncol = 2)
# sigmas = diag(sd)%*%cr%*%diag(sd)
# 
# class1 <- multi_rnorm(200, mus, sigmas)
# 
# mus <- c(-1, 2)
# sd <- c(1, 1)
# cr <- matrix(c(1, 0.2, 0.2, 1), ncol = 2)
# sigmas = diag(sd)%*%cr%*%diag(sd)
# 
# class2 <- multi_rnorm(200, mus, sigmas)
# 
# mus <- c(2, -2)
# sd <- c(1, 2)
# cr <- matrix(c(1, 0.3, 0.3, 1), ncol = 2)
# sigmas = diag(sd)%*%cr%*%diag(sd)
# 
# class3 <- multi_rnorm(200, mus, sigmas)
# 
# data2 = rbind(class1,class2,class3)
# data2 = cbind(data2,c(rep(1,200),rep(2,200),rep(3,200)))
# colnames(data2) = c("var1","var2","class")
# data2 = data.frame(data2)
# plot(data2[,1:2],col=data2[,3])

## Multinomial ---------------------
mult=multinom(class ~ var1+var2,data=data)
pred = predict(mult,data2)
pred = as.numeric(pred)
table(pred,data2$class)
mean(pred==data2$class)

#draw discrimination line
np <- 500
nd.var1 <- seq(from = min(data$var1), to = max(data$var1), length.out = np)
nd.var2 <- seq(from = min(data$var2), to = max(data$var2), length.out = np)
nd <- expand.grid(var1 = nd.var1, var2 = nd.var2)
dim(nd)
prd <- as.numeric(predict(mult, newdata = nd))
plot(data2[,1:2], col = data2$class)
contour(x = nd.var1, y = nd.var2, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2,3), add = TRUE, drawlabels = FALSE)


## KNN ----------------------------------
library(class)
acc=1:30

for (M in 1:30) {
  knn.pred=knn(data[,1:2],data2[,1:2],data$class ,k=M)
  table(knn.pred,data2$class)
  acc[M] = mean(knn.pred==data2$class)
}

acc
which.max(acc)
plot(acc,type='b')

# draw discrimination line
prd <- as.numeric(knn(data2[,1:2],nd,data2$class ,k=50))
plot(data2[,1:2], col = data2$class)
contour(x = nd.var1, y = nd.var2, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2,3), add = TRUE, drawlabels = FALSE)

## Compare multinom and KNN ------------------------
acc10 = matrix(rep(0,500),10,50)
for (i in 1:10){
  mus <- c(0, 3)
  sd <- c(2, 1)
  cr <- matrix(c(1, 0.5, 0.5, 1), ncol = 2)
  sigmas = diag(sd)%*%cr%*%diag(sd)
  
  class1 <- multi_rnorm(200, mus, sigmas)
  
  mus <- c(-1, 2)
  sd <- c(1, 1)
  cr <- matrix(c(1, 0.2, 0.2, 1), ncol = 2)
  sigmas = diag(sd)%*%cr%*%diag(sd)
  
  class2 <- multi_rnorm(200, mus, sigmas)
  
  mus <- c(2, -2)
  sd <- c(1, 2)
  cr <- matrix(c(1, 0.3, 0.3, 1), ncol = 2)
  sigmas = diag(sd)%*%cr%*%diag(sd)
  
  class3 <- multi_rnorm(200, mus, sigmas)
  
  data2 = rbind(class1,class2,class3)
  data2 = cbind(data2,c(rep(1,200),rep(2,200),rep(3,200)))
  colnames(data2) = c("var1","var2","class")
  data2 = data.frame(data2)
  acc=1:50
  for (M in 1:50) {
    knn.pred=knn(data[,1:2],data2[,1:2],data$class ,k=M)
    table(knn.pred,data2$class)
    acc[M] = mean(knn.pred==data2$class)
  }
  acc10[i,] = acc
}
acc10
acc.final = colMeans(acc10)
which.max(acc.final)

