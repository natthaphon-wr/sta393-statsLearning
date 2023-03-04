# Set Library ----------------------
library(ISLR)
library(leaps)
library(dplyr)
library(boot)


# Load and Clean Data --------------------
Auto <- Auto
summary(Auto)
Auto <- Auto %>% 
  select(-c(name)) %>% 
  relocate(mpg, .after = last_col())
summary(Auto)


# Build linear regression model -------------
model = lm(mpg~., data=Auto)
summary(model)
# Based on p-values, there are only displacement, weight, year, and origin
#   that should be predictors.


# Define selection method -----------------
regfit_best = regsubsets(mpg~., Auto, nvmax=7)
regfit_fwd = regsubsets(mpg~., Auto, nvmax=7, method="forward")
regfit_bwd = regsubsets(mpg~., Auto, nvmax=7, method="backward")
regfit_hyb = regsubsets(mpg~., Auto, nvmax=7, method="seqrep")
reg_summ_best = summary(regfit_best)
reg_summ_fwd = summary(regfit_fwd)
reg_summ_bwd = summary(regfit_bwd)
reg_summ_hyb = summary(regfit_hyb)


# Indirect Metrics ----------------------
## Cp --------------------------
which.min(reg_summ_best$cp)
which.min(reg_summ_fwd$cp)
which.min(reg_summ_bwd$cp)
which.min(reg_summ_hyb$cp)
coef(regfit_best, which.min(reg_summ_best$cp))
coef(regfit_fwd, which.min(reg_summ_fwd$cp))
coef(regfit_bwd, which.min(reg_summ_bwd$cp))
coef(regfit_hyb, which.min(reg_summ_hyb$cp))

## BIC -----------------------
which.min(reg_summ_best$bic)
which.min(reg_summ_fwd$bic)
which.min(reg_summ_bwd$bic)
which.min(reg_summ_hyb$bic)
coef(regfit_best, which.min(reg_summ_best$bic))
coef(regfit_fwd, which.min(reg_summ_fwd$bic))
coef(regfit_bwd, which.min(reg_summ_bwd$bic))
coef(regfit_hyb, which.min(reg_summ_hyb$bic))


# Direct Metrics -----------------------------
train_cv <- function(nmax, reg_summ, selection){
  cv_error = 1:nmax
  for (i in 1:nmax) {
    glm.fit = glm(mpg~.,data=Auto[,c(reg_summ$which[i, -1], TRUE)])
    set.seed(33)
    cv_error[i] = cv.glm(Auto[, c(reg_summ$which[i, -1], TRUE)], glm.fit, K=10)$delta[1]
  }
  plot(1:nmax, cv_error, type='b', main=selection)
  points(which.min(cv_error), cv_error[which.min(cv_error)], col="red", cex=2, pch=20)
  
  return(reg_summ$which[which.min(cv_error), -1])
}

col_best = train_cv(7, reg_summ_best, "Best Subset Selection")
col_fwd = train_cv(7, reg_summ_fwd, "Forward Stepwise Selection")
col_bwd = train_cv(7, reg_summ_bwd, "Backward Stepwise Selection")
col_hyb = train_cv(7, reg_summ_hyb, "Hybrid Stepwise Selection")
col_best
col_fwd
col_bwd
col_hyb


# Compare Indirect and Direct Metrics ------------------
# I choose hybrid stepwise selection from Cp metrics and CV for comparison.
#   The reason is that it's most different from the others.
# Hybrid from Cp: displacement, horsepower, weight, year, origin
# CV from CV    : weight, year, origin

## Hyb from Cp metrics ---------------------
model_cp = lm(mpg ~ displacement+horsepower+weight+year+origin, data=Auto)
summary(model_cp)

## Hyb from CV ------------------------
model_CV = lm(mpg ~ weight+year+origin, data=Auto)
summary(model_CV)

