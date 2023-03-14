# USArrests Dataset -----------------------
## Data preparation ---------------------
states = row.names(USArrests) 
states
names(USArrests )
USArrests[,1]
apply(USArrests , 2, mean)
apply(USArrests , 2, var)

## PCA ----------------------------
pr.out = prcomp(USArrests , scale=TRUE)
names(pr.out)
pr.out$rotation
pr.out$sdev
pr.out$x
pr.out$rotation = -pr.out$rotation  #change sign of rotation (loading)
pr.out$x = -pr.out$x
biplot(pr.out, scale=0)   
pr.var=pr.out$sdev ^2   
pr.var
pve = pr.var/sum(pr.var) #ratio of var explained
pve
plot(cumsum(pve))
par(mfrow=c(1,2))
plot(pve, xlab="Principal Component ", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component ", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
par(mfrow=c(1,1))


# College Dataset --------------------------------------
## Data preparation ---------------------
College = read.csv("College.csv", header=T, na.strings="?")
rownames(College) <- College[,1]
College = College[,-1]
head(College)
colSums(is.na(College))
dim(College)
#convert factor to numeric
College$Private = as.numeric(as.factor(College$Private))-1
summary(College)

## PCA --------------------------
pr.out = prcomp(College[,-ncol(College)], scale=TRUE)
names(pr.out)
pr.out$rotation 
pr.out$sdev
pr.out$x
biplot(pr.out, scale=0)   
pr.var = pr.out$sdev^2   
pr.var
pve = pr.var/sum(pr.var) #ratio of var explained
pve
cumpve = cumsum(pve)
plot(cumpve)

## PCA Regression -------------------------
### Using pls library ----------------------
library(pls)
pcr_fit_pls <- pcr(Grad.Rate~., data=College, scale=TRUE, validation="CV")
summary(pcr_fit_pls)
pcr_fit_pls$loadings
pcr_fit_pls$coefficients
t.grad = pcr_fit_pls$fitted.values
pcr_fit_pls$scores[,1]
pcr_fit_pls$model
summary(pcr_fit_pls)
validationplot(pcr_fit_pls)
RMSEP(pcr_fit_pls)
ncomp_pca_onesigma = selectNcomp(pcr_fit_pls, plot=TRUE, method="onesigma")
rmse_pca_onesigma = 13.38 #from adjCV rmse

# predplot(pcr_fit_pls, ncomp=13)
# pred_pls <- predict(pcr_fit_pls, College[1:10,], ncomp = 4)


### Manual method ---------------------------
library(boot)
cv.error = 1:17
ss = sample(1:10000)[1]
for (i in 1:17){
  College.pca = data.frame(cbind(pr.out$x[,1:i],College$Grad.Rate))
  colnames(College.pca)[ncol(College.pca)] = "GradRate"
  glm.fit = glm(GradRate~., data = College.pca)
  set.seed(ss)
  cv.error[i] = cv.glm(College.pca,glm.fit, K=10)$delta[1]
}
ncomp_pca_manual <- which.min(sqrt(cv.error))
rmse_pca_manual <- min(sqrt(cv.error))
rmse_pca_manual

# College.pca = data.frame(cbind(pr.out$x[,1:which.min(cv.error)], College$Grad.Rate))
# colnames(College.pca)[ncol(College.pca)] = "GradRate"
# glm.fit=glm(GradRate~., data = College.pca)
# summary(glm.fit)


## Lasso Regression ---------------------
# library(glmnet)
# x = model.matrix(Grad.Rate~.,College)[,-1]
# y = College$Grad.Rate
# dim(x)
# length(y)
# 
# # alpha = 1 -> lasso, alpha=0 -> ridge
# build_model <- function(alpha, x, y){
#   grid = 10^seq(10,-2, length=100)
#   grid = c(grid,0)
#   model = glmnet(x, y, alpha=alpha, lambda=grid)
#   return(model)
# }
# 
# find_best_lamb <- function(alpha, model, x, y, k){
#   cv.out = cv.glmnet(x, y, alpha=alpha, nfolds=k)
#   best_lamb = cv.out$lambda.min
#   cv.error = cv.glm(x, y, cv.out, K=10)$delta[1]
#   # print(cv.error)
#   return(best_lamb)
# }
# 
# las_model = build_model(alpha=1, x, y)
# best_lamb = find_best_lamb(alpha=1, model=las_model, x=x, y=y, k=10)


## Subset Selection ------------------------------------
library(bestglm)
predict.regsubsets = function(object ,newdata ,id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}





## Normal method --------------------
glm.fit.ori = glm(Grad.Rate~., data = College)
summary(glm.fit.ori)
set.seed(ss)
rmse_normal = sqrt(cv.glm(College, glm.fit.ori, K=10)$delta[1])
rmse_normal










## Lasso on PCA -------------------------------
# grid=10^seq(10,-2, length=100)
# grid = c(grid,0)
# lasso.mod=glmnet(x,y,alpha=1,lambda=grid)  #alpha*L1(beta)+(1-alpha)*L2(beta)
# College.pca = data.frame(cbind(pr.out$x,College$Grad.Rate))
# colnames(College.pca)[ncol(College.pca)] = "GradRate"
# x=model.matrix(GradRate~.,College.pca)[,-1]
# head(x)
# y=College.pca$GradRate
# 
# set.seed(ss)
# cv.out2=cv.glmnet(x,y,alpha=1,nfolds=10)
# names(cv.out2)
# bestlam2 = cv.out2$lambda.min
# bestlam2
# 
# plot(cv.out2$lambda,cv.out2$cvm,type = 'b')
# 
# #Try more lambda
# 
# set.seed(ss)
# cv.out=cv.glmnet(x,y,alpha=1,lambda = seq(0.10,0.50,0.001),nfolds=10)
# bestlam2 = cv.out$lambda.min
# sqrt(min(cv.out$cvm))
# predict(lasso.mod,s=bestlam2,type="coefficients")[1:18,]
# 
# plot(cv.out$lambda,sqrt(cv.out$cvm),type = 'b', pch=20)



