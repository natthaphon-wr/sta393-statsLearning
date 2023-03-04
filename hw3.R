# Set Environment/Library ----------------------
library(caret)
library(glmnet)
library(tictoc)
library(readr)
library(gplots)
library(ggplot2)


# Import Data ----------------------------------
BankChurners <- read_csv("BankChurners.csv")
attach(BankChurners)

# Proposition ----------------------------------
# Consider following attribute to using linear regression to predict "Total Revolving Balance"
#   : Customer_Age, Gender, Dependent_count, Education_level, Marital_Status, 
#     Income_Category, Months_on_book, Credit_Limit


# Linear Regression Requirement -----------------
# When using linear regression, the attributes must be numerical data, so
#   there are some attributes that can use.
# There are Customer_Age, Dependent_count, Months_on_book, and Credit_Limit


# Find Good Model/Attribute to used ----------------
## Plot graph to see relationship ----------------
plot(Customer_Age, Total_Revolving_Bal,
     main = "Data Exploration",
     xlab = "Customer Age",
     ylab = "Total Revolving Balance")

plot(Dependent_count, Total_Revolving_Bal,
     main = "Data Exploration",
     xlab = "Dependent Count",
     ylab = "Total Revolving Balance")

plot(Months_on_book, Total_Revolving_Bal,
     main = "Data Exploration",
     xlab = "Months on Book",
     ylab = "Total Revolving Balance")

plot(Credit_Limit, Total_Revolving_Bal,
     main = "Data Exploration",
     xlab = "Credit Limit",
     ylab = "Total Revolving Balance")

# Dependent Count isn't numerical (continuous) data too, so the remaining attribute are
#   Customer_Age, Months_on_book, and Credit_Limit
# According to 3 graph above, they aren't showed linear relationship, so I will 
#   try to fit various models for finding good model


## Find Correlation between variable -----------------
cor(Customer_Age, Months_on_book)
cor(Customer_Age, Credit_Limit)
cor(Months_on_book, Credit_Limit)
# See that correlation of customer_age and months_on_book is quite height (0.79), and
#   it's also make sense.


## Try Various Model --------------------------------
model1 <- lm(Total_Revolving_Bal ~ Customer_Age+Months_on_book+Credit_Limit)
summary(model1)
model2 <- lm(Total_Revolving_Bal ~ Customer_Age*Months_on_book+Credit_Limit)
summary(model2)

# See that model2 (have interacton mode) is doing better than model1, but
#   both models are not good enough. From model2, there is only 1 attribute that
#   is significant P-value, it's Credit_Limit

model3 <- lm(Total_Revolving_Bal ~ Customer_Age)
summary(model3)
model4 <- lm(Total_Revolving_Bal ~ Months_on_book)
summary(model4)

model5 <- lm(Total_Revolving_Bal ~ Credit_Limit)
summary(model5)



# From using single linear regression, it's founded that Credit_Limit is only significant attribute
# Next, I will try using polynomial regression with Credit_Limit

rsq_list = list()
for (i in 1:10){
  model6 <- lm(Total_Revolving_Bal ~ poly(Credit_Limit, i))
  rsq = summary(model6)$r.squared
  rsq_list <- append(rsq_list, rsq)
}
plot(1:10, rsq_list,
     main="Polynomial Regression of Credit Limit",
     xlab="Polynomial Degree",
     ylab="R Squared")
sprintf("The Maximum R Squared is %f at %d polynomal degree", 
        max(unlist(rsq_list)), which.max(rsq_list))

# See that the best model is 10 degree polynomial regression of Credit_Limit, 
#   according to Adjusted R-squared Although, it's not much (0.129452), but it can used for
#   comparing CV method. (cross validation isn't the main process to find/improve the model)


# K-fold (10-fold) Method ---------------------------
## Setup k-fold -------------------
k_control <- trainControl(method = "CV", number = 10)  #10-fold

## Fit Model --------------------
model_k <- train(Total_Revolving_Bal ~ poly(Credit_Limit, 10),
                 data = BankChurners, 
                 method = "lm",
                 trControl = k_control)


# Monte Carlo Method ----------------------------------------------
monte_func <- function(ITERATIONS, ratio){
  ## Setup Monte Carlo -------------------
  # Leave-Group-Out-Cross-Validation (LGOCV) is same as Monte Carlo CV
  monte_control <- trainControl(method = "LGOCV", number = ITERATIONS, p=ratio)
  
  ## Fit Model ------------------------
  model_monte <- train(Total_Revolving_Bal ~ poly(Credit_Limit, 10),
                 data = BankChurners,
                 method = "lm",
                 trControl = monte_control)
  
  return(model_monte)
}

## Call function -----------------------
model_monte10 = monte_func(10, 0.9)
model_monte100 = monte_func(100, 0.9)
model_monte1000 = monte_func(1000, 0.9)
model_monte10000 = monte_func(1000, 0.9)


# Comparison result ----------------------------
boxplot(model_k$resample$RMSE, 
        model_monte10$resample$RMSE,
        model_monte100$resample$RMSE,
        model_monte1000$resample$RMSE,
        model_monte10000$resample$RMSE,
        names = c("10-fold", "MC 10", "MC 100", "MC 1000", "MC 10000"),
        main = "Boxplot for Comparison of Variance",
        ylab = "RMSE"
        )

IQR(model_k$resample$RMSE)
IQR(model_monte10$resample$RMSE)
IQR(model_monte100$resample$RMSE)
IQR(model_monte1000$resample$RMSE)
IQR(model_monte10000$resample$RMSE)

