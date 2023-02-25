# Set library --------------------------------------------
library(datasets)
library(caret)
library(glmnet)
library(ggplot2)
library(nnet)
library(class)
library(dplyr)


# Load Iris and explore dataset --------------------------------------------
data(iris)
summary(iris)
sapply(iris, class)
contrasts(iris$Species)
speciesLV <- levels(iris$Species)


# Logistic Regression Model ----------------------------------------
## Find optimal independent variables ---------------------
model_logit <- glm(Species~., data = iris, family = "binomial")
summary(model_logit)
# It's show that dependent variable is perfectly separate from independent variables.
# Then, I will explore more data in each independent variable.
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + 
  geom_point(size=2) 
ggplot(iris, aes(x=Petal.Length, y=Petal.Width, color=Species)) + 
  geom_point(size=2) 
# From petal graph (2nd), it's show that both Width and Length are perfectly separate 
#   from Species. Moreover, sepal.width and sepal.length are quite separate perfectly, 
#   so I will make boxplot.
boxplot(Sepal.Width~Species, data=iris, main="Boxplot of Sepal Width")
boxplot(Sepal.Length~Species, data=iris,  main="Boxplot of Sepal Length")
# From both graph, it's not show much different, but I think Sepal.length is too
#   perfectly separate from Species, so I will only use sepal.width as independent variables
 
## k-fold setting ---------------------------
k = 10
folds = sample(1:k, dim(iris)[1], replace=TRUE)
acc_logit_all = list()
acc_logit_se = list()
acc_logit_ve = list()
acc_logit_vi = list()

for(j in 1:k){
  ## Fit model ----------------------
  model_multinom <- multinom(Species~Sepal.Width, data = iris, subset=(folds != j))
  summary(model_multinom)
  
  ## Predict --------------
  pred = predict(model_multinom, iris[folds == j,])
  
  ## Confustion matrix and calculate accuracy ----------------------------
  conf_matrix <- table(pred, iris$Species[folds == j])
  acc_all <- mean(pred == iris$Species[folds == j])
  acc_se <- conf_matrix[1,1]/(conf_matrix[1,1] + conf_matrix[2,1] + conf_matrix[3,1])
  acc_ve <- conf_matrix[2,2]/(conf_matrix[1,2] + conf_matrix[2,2] + conf_matrix[3,2])
  acc_vi <- conf_matrix[3,3]/(conf_matrix[1,3] + conf_matrix[2,3] + conf_matrix[3,3])
  
  acc_logit_all <- append(acc_logit_all, acc_all)
  acc_logit_se <- append(acc_logit_se, acc_se)
  acc_logit_ve <- append(acc_logit_ve, acc_ve)
  acc_logit_vi <- append(acc_logit_vi, acc_vi)
  
}

## Final Result -----------------------------
avgacc_logit_all <- mean(unlist(acc_logit_all))
avgacc_logit_se <- mean(unlist(acc_logit_se))
avgacc_logit_ve <- mean(unlist(acc_logit_ve))
avgacc_logit_vi <- mean(unlist(acc_logit_vi))


# KNN Model ----------------------------------------------
## k-fold setting ---------------------------
k = 10
folds = sample(1:k, dim(iris)[1], replace=TRUE)
acc_knn_all = list()
acc_knn_se = list()
acc_knn_ve = list()
acc_knn_vi = list()

for(j in 1:k){
  ## Predict with KNN model ---------------------
  knn_pred = knn(iris[folds != j,-ncol(iris)],
                 iris[folds == j,-ncol(iris)],
                 iris$Species[folds != j],
                 k=10)
  
  ## Confustion matrix and calculate accuracy --------------------------
  conf_matrix2 = table(knn_pred, iris$Species[folds == j])
  
  acc_all <- mean(knn_pred == iris$Species[folds == j])
  acc_se <- conf_matrix2[1,1]/(conf_matrix2[1,1] + conf_matrix2[2,1] + conf_matrix2[3,1])
  acc_ve <- conf_matrix2[2,2]/(conf_matrix2[1,2] + conf_matrix2[2,2] + conf_matrix2[3,2])
  acc_vi <- conf_matrix2[3,3]/(conf_matrix2[1,3] + conf_matrix2[2,3] + conf_matrix2[3,3])
  
  acc_knn_all <- append(acc_knn_all, acc_all)
  acc_knn_se <- append(acc_knn_se, acc_se)
  acc_knn_ve <- append(acc_knn_ve, acc_ve)
  acc_knn_vi <- append(acc_knn_vi, acc_vi)

}

## Final Result -----------------------------
avgacc_knn_all <- mean(unlist(acc_knn_all))
avgacc_knn_se <- mean(unlist(acc_knn_se))
avgacc_knn_ve <- mean(unlist(acc_knn_ve))
avgacc_knn_vi <- mean(unlist(acc_knn_vi))


