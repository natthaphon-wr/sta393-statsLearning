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
library(glmnet)
x = model.matrix(Grad.Rate~.,College)[,-1]
y = College$Grad.Rate
dim(x)
length(y)

# alpha = 1 -> lasso, alpha=0 -> ridge
build_model <- function(alpha, x, y){
  grid = 10^seq(10,-2, length=100)
  grid = c(grid,0)
  model = glmnet(x, y, alpha=alpha, lambda=grid)
  return(model)
}

find_best_lamb <- function(alpha, model, x, y, k){
  cv.out = cv.glmnet(x, y, alpha=alpha, nfolds=k)
  best_lamb = cv.out$lambda.min
  mse_min <- cv.out$cvm[cv.out$lambda == cv.out$lambda.min]
  return(list(best_lamb, mse_min))
}

las_model = build_model(alpha=1, x, y)
result = find_best_lamb(alpha=1, model=las_model, x=x, y=y, k=10)
best_lamb = result[[1]]
rmse_lasso = sqrt(result[[2]])


## Subset Selection ------------------------------------
library(bestglm)
predict.regsubsets = function(object ,newdata ,id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

reg_cv <- function(nvmax, reg_fit, reg_summ){
  cv.error = 1:nvmax
  for (i in 1:nvmax){
    glm.fit = glm(Grad.Rate~., data=College[, c(reg_summ$which[i,-1], TRUE)])
    cv.error[i] = cv.glm(College[, c(reg_summ$which[i,-1],TRUE)], glm.fit,K=10)$delta[1]
  }
  return(min(cv.error))
}

regfit_best = regsubsets(Grad.Rate~., College, nvmax=17)
regfit_fwd = regsubsets(Grad.Rate~., College, nvmax=17, method="forward")
regfit_bwd = regsubsets(Grad.Rate~., College, nvmax=17, method="backward")
regfit_hyb = regsubsets(Grad.Rate~., College, nvmax=17, method="seqrep")
reg_summ_best = summary(regfit_best)
reg_summ_fwd = summary(regfit_fwd)
reg_summ_bwd = summary(regfit_bwd)
reg_summ_hyb = summary(regfit_hyb)

error_regbest = reg_cv(17, regfit_best, reg_summ_best)
rmse_regbest = sqrt(error_regbest)
error_regfwd = reg_cv(17, regfit_fwd, reg_summ_fwd)
rmse_regfwd = sqrt(error_regfwd)
error_regbwd = reg_cv(17, regfit_bwd, reg_summ_bwd)
rmse_regbwd = sqrt(error_regbwd)
error_reghyb = reg_cv(17, regfit_hyb, reg_summ_hyb)
rmse_reghyb = sqrt(error_reghyb)


## Normal method --------------------
glm.fit.ori = glm(Grad.Rate~., data = College)
summary(glm.fit.ori)
set.seed(ss)
rmse_normal = sqrt(cv.glm(College, glm.fit.ori, K=10)$delta[1])
rmse_normal


# Conclusion and Discussion -------------------------
sprintf("RMSE of Normal Method: %f", rmse_normal)
sprintf("RMSE of Best Subset Selection Method: %f", rmse_regbest)
sprintf("RMSE of Forward Stepwise Selection Method: %f", rmse_regfwd)
sprintf("RMSE of Backward Stepwise Selection Method: %f", rmse_regbwd)
sprintf("RMSE of Hybrid Stepwise Selection Method: %f", rmse_reghyb)
sprintf("RMSE of Lasso Regression Method: %f", rmse_lasso)
sprintf("RMSE of PCA with Onesigma Method: %f", rmse_pca_onesigma)
sprintf("RMSE of PCA with Minimum Manual Method: %f", rmse_pca_manual)

# From those result, the minimum RMSE is 12.8376 from "Backward Stepwise Selection".
#   However, all RMSE value aren't different much. There isn't enough evidence to conclude
#   that Backward Stepwise Selection is the best method for this problem. 
#   (Maybe RMSE aren't significant different). 
#   The main point from this experiment is that "There are various method for 
#   independence variable selection, and they give (slightly) different result."







