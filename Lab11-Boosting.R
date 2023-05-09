library(tree)
library(MASS)
?Boston
head(Boston)
set.seed(166)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston ,subset=train) #
summary(tree.boston)
plot(tree.boston)
text(tree.boston ,pretty=0) 

?gbm

#Boosting
library(gbm)
set.seed(166)
?gbm
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000, interaction.depth=4) #lambda=0.01
summary(boost.boston)
par(mfrow=c(1,2))


boston.test = Boston$medv[-train]
yhat.boost=predict(boost.boston ,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost -boston.test)^2)

boost.boston=gbm(medv~.,data=Boston[train ,],distribution="gaussian",n.trees=5000, interaction.depth=6,shrinkage = 0.1,verbose=F)
yhat.boost=predict(boost.boston ,newdata=Boston[-train ,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

yhat.boost=predict(boost.boston ,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost -boston.test)^2)

k=10
set.seed(123)
folds=sample(1:k,nrow(Boston),replace=TRUE)


ll = seq(0.005,0.01,0.001)
rmse.ll = vector()
for (i in 1:length(ll)){
  cv.mse.boost= 1:k
  for(j in 1:k)
  {
    boost.boston=gbm(medv~.,data=Boston[folds!=j,],distribution="gaussian",n.trees=5000, interaction.depth=4,shrinkage = 0.01,verbose=F)
    cv.mse.boost[j]=mean((Boston$medv[folds==j]-predict(boost.boston ,newdata=Boston[folds==j,], n.trees=5000))^2)
  }
  rmse.ll[i] = mean(cv.mse.boost)
  
}
rmse.ll
ll[3]

boost.boston=gbm(medv~.,data=Boston,distribution="gaussian",n.trees=5000, interaction.depth=4,shrinkage = 0.07,verbose=F)
summary.gbm(boost.boston)

plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")
plot(boost.boston ,i="dis")
plot(boost.boston ,i="crim")
plot(boost.boston ,i="nox")
plot(boost.boston ,i="age")
plot(boost.boston ,i="black")
plot(boost.boston ,i="ptratio")
