# Set Library -------------------------------------
library(ISLR)
library(leaps)
library(dplyr)
library(boot)
library(readr)


# Define Problem ----------------------------- 
# For this problem, I will compare with HW3 that use linear regression model to predict 
#   "Total Revolving Balance", and I will compare accuracy as "residual standard error" and
#   "Adjusted R-squared".


# Load and Prepare Data ---------------------------------------
BankChurners <- read_csv("BankChurners.csv")
BankChurners <- BankChurners %>% 
  select(where(is.numeric)) %>% 
  relocate(Total_Revolving_Bal, .after = last_col())
summary(BankChurners)
# Remove CLIENTNUM column that's id
BankChurners <- BankChurners %>% 
  select(-c(CLIENTNUM))
summary(BankChurners)


# Define selection method -----------------
regfit_best = regsubsets(Total_Revolving_Bal~., BankChurners, nvmax=13)
regfit_fwd = regsubsets(Total_Revolving_Bal~., BankChurners, nvmax=13, method="forward")
regfit_bwd = regsubsets(Total_Revolving_Bal~., BankChurners, nvmax=13, method="backward")
regfit_hyb = regsubsets(Total_Revolving_Bal~., BankChurners, nvmax=13, method="seqrep")
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
    glm.fit = glm(Total_Revolving_Bal~., data=BankChurners[,c(reg_summ$which[i, -1], TRUE)])
    set.seed(33)
    cv_error[i] = cv.glm(BankChurners[, c(reg_summ$which[i, -1], TRUE)], glm.fit, K=10)$delta[1]
  }
  plot(1:nmax, cv_error, type='b', main=selection)
  points(which.min(cv_error), cv_error[which.min(cv_error)], col="red", cex=2, pch=20)
  
  return(reg_summ$which[which.min(cv_error), -1])
}

col_best = train_cv(13, reg_summ_best, "Best Subset Selection")
col_fwd = train_cv(13, reg_summ_fwd, "Forward Stepwise Selection")
col_bwd = train_cv(13, reg_summ_bwd, "Backward Stepwise Selection")
col_hyb = train_cv(13, reg_summ_hyb, "Hybrid Stepwise Selection")
col_best
col_fwd
col_bwd
col_hyb

# Compare Final Model -------------------------------
## From HW3 Linear Regression ------------------------------
model_hw3 <- lm(Total_Revolving_Bal ~ Credit_Limit, data=BankChurners)
summary(model_hw3)

## Hyb from CV ------------------------
model_CV = lm(Total_Revolving_Bal ~ Customer_Age+Total_Relationship_Count+
                Months_Inactive_12_mon+Contacts_Count_12_mon+Credit_Limit+Avg_Open_To_Buy+
                Total_Amt_Chng_Q4_Q1+Total_Trans_Ct+Avg_Utilization_Ratio, 
              data=BankChurners)
summary(model_CV)


