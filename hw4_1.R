# Set library --------------------------------------------
library(caret)
library(glmnet)
library(readr)
library(ggplot2)
library(dplyr)
library(DMwR2)
library(tidyverse)
library(broom)
library(ROSE)
library(pscl)
library(class)

# Import and explore data --------------------------------------------
BankChurners <- read_csv("BankChurners.csv")
summary(BankChurners)
table(BankChurners$Attrition_Flag)

## Change character variables to factor ------------------
sapply(BankChurners, class)
BankChurners <- BankChurners %>% mutate_if(is.character, as.factor)
sapply(BankChurners, class)
summary(BankChurners)

## # Refactor reference level ------------------------
contrasts(BankChurners$Attrition_Flag)
# The objective is to predict attrited customer, so I change it. 
BankChurners$Attrition_Flag <- relevel(BankChurners$Attrition_Flag, ref = 2)
contrasts(BankChurners$Attrition_Flag)


# Logistic Regression -----------------------
## Independent variables selection -------------
# The main assumptions of logistic regression are independent variables must continuous,
#   and haven't multicollinearity(contain highly correlated independent variables).
#   So, the remaining variables that I think can use (including business rational) are 
#     1. Total_relationship_count
#     2. Contacts_count_12_mon
#     3. Avg_open_to_buy,
#     4. Total_trans_amt
#     5. Avg_utilization_ratio

## k-fold setting ---------------------------
k = 10
folds = sample(1:k, dim(BankChurners)[1], replace=TRUE)
prec_list = list()
rec_list = list()
f1_list = list()
acc_list = list()

## Loop for CV ----------------------
for(j in 1:k){
  ## Fit model ----------------------
  model_logit <- glm(Attrition_Flag ~ Total_Relationship_Count+Contacts_Count_12_mon+ 
                       Avg_Open_To_Buy+Total_Trans_Amt+Avg_Utilization_Ratio, 
                     data = BankChurners, 
                     subset= (folds != j),
                     family = "binomial")
    
  ## Set threshold and predict --------------
  probs = predict(model_logit, newdata = BankChurners[folds == j,], type = "response")
  pred_class = ifelse(probs > 0.4, "Attrited Customer", "Existing Customer")
  
  ## See result ----------------------------
  conf_matrix1 = table(pred_class, 
                       factor(BankChurners$Attrition_Flag[folds == j], 
                              levels=c("Attrited Customer", "Existing Customer"))
                       )
  prec = conf_matrix1[1,1]/(conf_matrix1[1,1] + conf_matrix1[1,2]) #precision
  rec = conf_matrix1[1,1]/(conf_matrix1[1,1] + conf_matrix1[2,1]) #recall or TPR
  f1 = 2*prec*rec/(prec+rec) #f1-score
  acc = mean(pred_class == BankChurners$Attrition_Flag[folds == j]) #accuracy
  
  prec_list <- append(prec_list, prec)
  rec_list <- append(rec_list, rec)
  f1_list <- append(f1_list, f1)
  acc_list <- append(acc_list, acc)
  
}

## Final Result -----------------------------
avg_prec_logit <- mean(unlist(prec_list))
avg_rec_logit <- mean(unlist(rec_list))
avg_f1_logit <- mean(unlist(f1_list))
avg_acc_logit <- mean(unlist(acc_list))

## See example pseudo R-squared and ROC curve from last model ------------
pR2(model_logit)
roc.curve(BankChurners$Attrition_Flag[folds == j], probs)


# KNN model ---------------------------------
# I will use independent variables same as logistic regression for comparison.
# Using k=10

## k-fold and KNN setting ---------------------------
BankChurners_knn <- BankChurners %>% 
  select(Total_Relationship_Count, Contacts_Count_12_mon, Avg_Open_To_Buy, 
         Total_Trans_Amt, Avg_Utilization_Ratio, Attrition_Flag)
contrasts(BankChurners_knn$Attrition_Flag)
summary(BankChurners_knn)

k = 10
folds = sample(1:k, dim(BankChurners_knn)[1], replace=TRUE)
prec_list_knn = list()
rec_list_knn = list()
f1_list_knn = list()
acc_list_knn = list()

## Loop for CV ----------------------
for(j in 1:k){
  #print(dim(BankChurners_knn[folds != j,-ncol(BankChurners_knn)]))
  #print(dim(BankChurners_knn[folds == j,-ncol(BankChurners_knn)]))
  
  ## Predict with KNN model ----------------------
  knn_pred = knn(BankChurners_knn[folds != j,-ncol(BankChurners_knn)], 
                 BankChurners_knn[folds == j,-ncol(BankChurners_knn)], 
                 BankChurners_knn$Attrition_Flag[folds != j], 
                 k=10)
  
  ## See result ----------------------------
  conf_matrix2 = table(factor(knn_pred, 
                              levels=c("Attrited Customer", "Existing Customer")), 
                       factor(BankChurners_knn$Attrition_Flag[folds == j],
                              levels=c("Attrited Customer", "Existing Customer"))
                       )
  
  prec = conf_matrix2[1,1]/(conf_matrix2[1,1] + conf_matrix2[1,2]) #precision
  rec = conf_matrix2[1,1]/(conf_matrix2[1,1] + conf_matrix2[2,1]) #recall or TPR
  f1 = 2*prec*rec/(prec+rec) #f1-score
  acc = mean(knn_pred == BankChurners$Attrition_Flag[folds == j]) #accuracy
  
  prec_list_knn <- append(prec_list_knn, prec)
  rec_list_knn <- append(rec_list_knn, rec)
  f1_list_knn <- append(f1_list_knn, f1)
  acc_list_knn <- append(acc_list_knn, acc)
}

## Final Result -----------------------------
avg_prec_knn <- mean(unlist(prec_list_knn))
avg_rec_knn <- mean(unlist(rec_list_knn))
avg_f1_knn <- mean(unlist(f1_list_knn))
avg_acc_knn <- mean(unlist(acc_list_knn))


