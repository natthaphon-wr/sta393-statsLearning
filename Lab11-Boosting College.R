# Import Library ---------------------------------------------------------------
library(tree)
library(MASS)
library(tictoc)
library(gbm)

library("xgboost")  # the main algorithm
library("archdata") # for the sample dataset
library("caret")    # for the confusionmatrix() function (also needs e1071 package)
library("dplyr")    # for some data preperation
library("Ckmeans.1d.dp") # for xgb.ggplot.importance


# Data Preparation -------------------------------------------------------------
college=read.csv("College.csv",header=T,na.strings="?")
rownames(college) = college[,1]
college=data.frame(college[,-1])
college$Private = as.factor(college$Private)
head(college)
dim(college)
sum(is.na(college))
train = sample(1:nrow(college), nrow(college)/2)
college.test = college[-train,ncol(college)]


# Regression Trees -------------------------------------------------------------

## Boosting ------------------------------------------------------

### Interaction.depth = 2 -------------------------------------
set.seed(166)
boost.college = gbm(Grad.Rate~., data=college[train,], distribution="gaussian",
                    n.trees=5000, interaction.depth=2, shrinkage=0.01,)
summary(boost.college)
names(college)
par(mfrow=c(1,2))
plot(boost.college ,i="Outstate")
plot(boost.college ,i="Top10perc")

yhat.boost = predict(boost.college, newdata=college[-train,], n.trees=5000)
sqrt(mean((yhat.boost -college.test)^2))

### Interaction.depth = 6 -------------------------------------
boost.college = gbm(Grad.Rate~., data=college[train ,],distribution="gaussian",
                    n.trees=5000, interaction.depth=6, shrinkage = 0.01)
yhat.boost = predict(boost.college ,newdata=college[-train ,],n.trees=5000)
sqrt(mean((yhat.boost-college.test)^2))


### Tuning Parameter ------------------------------------------
K = 10
folds = sample(1:K, nrow(college), replace=TRUE)
ll = seq(0.001, 0.01, by=0.001) #shrinkage (learning rate)
dd = 1:5                        #interaction.depth
rmse.para = matrix(rep(0,length(ll)*length(dd)),length(ll),length(dd))
dim(rmse.para)

for (i in 1:length(ll)){
  for (j in 1:length(dd)){
    # tic()
    cv.rmse.boost= 1:K
    for(k in 1:K)
    {
      boost.college = gbm(Grad.Rate~., data=college[folds!=k,], distribution="gaussian",
                          n.trees=5000, interaction.depth=dd[j], shrinkage = ll[i])
      cv.rmse.boost[k] = sqrt(mean((college$Grad.Rate[folds==k]-
                                      predict(boost.college,newdata=college[folds==k,],n.trees=5000))^2))
    }
    rmse.para[i,j] = mean(cv.rmse.boost)
    # toc()
  }
}
rmse.para
boost_rmse <- min(rmse.para)
boost_learnRate <- which(rmse.para == min(rmse.para), arr.ind = TRUE)[1]
boost_interactDepth <- which(rmse.para == min(rmse.para), arr.ind = TRUE)[2]


## XGBoost ----------------------------------------------------

### Data Preparation ---------------------------
dat <- college
dat$Private = as.numeric(dat$Private)
dat$Grad.Rate = as.numeric(dat$Grad.Rate)

K = 10
set.seed(1)
folds = sample(1:K,nrow(college),replace=TRUE)

data_variables <- as.matrix(dat[,-ncol(dat)])
data_label <- dat[,ncol(dat)]
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables), label = data_label)


### Tuning Parameter -------------------------
eee = seq(0.1,0.5,0.05) #learning rate
mdd = 4:8               #max depth
mchw = 1:5              #min child weight
rmse_df = data.frame(matrix(ncol=4,nrow=0, 
                            dimnames=list(NULL, c("learnRate", "maxDepth", "minChild", "rmse"))))

# rmse.xgb = matrix(rep(0,length(eee)*length(mdd)), length(eee), length(mdd))

for (ee in 1:length(eee)){
  for (md in 1:length(mdd)){
    for (mcw in 1:length(mchw)){
      # tic()
      xgb_params <- list("eta" = eee[ee], 
                         "max_depth" = mdd[md],
                         "min_child_weight" = mchw[mcw],
                         "objective" = "reg:squarederror",
                         "eval_metric" = "rmse")
      nround <- 20
      
      cv.errors = 1:K
      for (j in 1:K){
        # split train data and make xgb.DMatrix
        train_data   <- data_variables[folds!=j,]
        train_label  <- data_label[folds!=j]
        train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
        
        # split test data and make xgb.DMatrix
        test_data  <- data_variables[folds==j,]
        test_label <- data_label[folds==j]
        test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
        
        # train model
        bst_model <- xgb.train(params = xgb_params,
                               data = train_matrix,
                               nrounds = nround)
        
        # Predict hold-out test set
        test_pred <- predict(bst_model, newdata = test_matrix)
        cv.errors[j] = sqrt(mean((test_label-test_pred)^2))
      }
      rmse_df[nrow(rmse_df) + 1,] = c(eee[ee], mdd[md], mchw[mcw], mean(cv.errors))
      #rmse.xgb[ee,md] = mean(cv.errors)
      # toc()
    }
  }
}

rmse_best <- min(rmse_df$rmse)
best_index <- which.min(rmse_df$rmse)
tuneParams <- rmse_df[best_index,]
tuneParams

# rmse.xgb
# loc = which(rmse.xgb==min(rmse.xgb),arr.ind = T)

### Best Parameter ------------------------------------
learnRate_best <- tuneParams$learnRate
maxDepth_best <- tuneParams$maxDepth
minChild_best <- tuneParams$minChild

rmse_df[rmse_df$learnRate==learnRate_best & rmse_df$maxDepth==maxDepth_best,]

xgb_params <- list("eta" = learnRate_best, 
                   "max_depth" = maxDepth_best,
                   "min_child_weight" = minChild_best,
                   "objective" = "reg:squarederror",
                   "eval_metric" = "rmse")
nround <- 20 

# split train data and make xgb.DMatrix
train_data   <- data_variables
train_label  <- data_label
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)

# get the feature real names
names <-  colnames(dat[,-ncol(dat)])

# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix)

gp = xgb.ggplot.importance(importance_matrix)
print(gp) 


# XGBoost classification -------------------------------------------------------

college2 = college
college2$Grad.Rate[college2$Grad.Rate>=70] = "High"
college2$Grad.Rate[college2$Grad.Rate<70] = "Low"
head(college2)
dim(college2)
college2$Private = as.factor(college2$Private)
college2$Grad.Rate = factor(college2$Grad.Rate,levels=c("Low","High"),ordered =T)
class(college2$Grad.Rate)

dat <- college2
head(dat)

dat$Private = as.numeric(dat$Private)
dat$Grad.Rate = as.numeric(dat$Grad.Rate)-1


K=10
set.seed(1)
folds=sample(1:K,nrow(dat),replace=TRUE)


# Full data set
data_variables <- as.matrix(dat[,-ncol(dat)])
data_label <- dat[,ncol(dat)]
data_matrix <- xgb.DMatrix(data = as.matrix(data_variables), label = data_label)


numberOfClasses <- length(unique(dat$Grad.Rate))

eee = seq(0.1,0.3,0.1)
mdd = 3:8
ppp = seq(0.3,0.7,0.1)

acc3 = matrix(6*length(eee)*length(mdd)*length(ppp),length(eee)*length(mdd)*length(ppp),6)

s=1
for (pp in ppp){
  for (ee in eee){
    for (md in mdd){
      tic()
      xgb_params <- list("eta" = ee, "max_depth" = md , "objective" = "multi:softprob",
                         "eval_metric" = "mlogloss",
                         "num_class" = numberOfClasses)
      nround    <- 50 # number of XGBoost rounds
      
      cv.errors = 1:K
      cvng = 1:K
      cvg = 1:K
      for (j in 1:K){
        # split train data and make xgb.DMatrix
        train_data   <- data_variables[folds!=j,]
        train_label  <- data_label[folds!=j]
        train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
        # split test data and make xgb.DMatrix
        test_data  <- data_variables[folds==j,]
        test_label <- data_label[folds==j]
        test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
        
        bst_model <- xgb.train(params = xgb_params,
                               data = train_matrix,
                               nrounds = nround)
        
        # Predict hold-out test set
        test_pred <- predict(bst_model, newdata = test_matrix)
        test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                                  ncol=length(test_pred)/numberOfClasses) %>%
          t() %>%
          data.frame() %>%
          mutate(label = test_label + 1,
                 max_prob = max.col(., "last"))
        # confusion matrix of test set
        predprob = rep(2,dim(test_data)[1])
        predprob[test_prediction[,1]>pp] = 1
        tb = table(test_prediction$label,predprob)
        cv.errors[j] = mean(test_prediction$label==predprob)
        cvng[j]  = tb[1,1]/sum(tb[1,])
        cvg[j]  = tb[2,2]/sum(tb[2,])
      }
      acc3[s,1] = mean(cv.errors)
      acc3[s,2] = mean(cvng)
      acc3[s,3] = mean(cvg)
      acc3[s,4] = ee
      acc3[s,5] = md
      acc3[s,6] = pp
      s = s+1
      toc()
    }
  }
}

dim(acc3)
which.max(acc3[,1])
acc3[which.max(acc3[,1]),]

acc3 = cbind(acc3,rep(0,300),rep(0,300),rep(0,300))

s=1
for (pp in seq(0.5,0.59,0.01)){
  for (ee in seq(0.1,0.5,0.1)){
    for (md in 3:8){
      acc3[s,4] = pp
      acc3[s,5] = ee
      acc3[s,6] = md
      s = s+1
    }
  }
}
acc3[acc3[,1]>0.72,]

#Collect highest acc and highest almost balanced accuracy

#0.7190095 0.7078767 0.7330375 0.56  0.1    4
#0.7121213 0.7166747 0.7054285 0.55  0.1    4


confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

ee=0.5
md = 3
pp = 0.5

xgb_params <- list("eta" = ee, "max_depth" = md , "objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds

tblist = list()
cv.errors = 1:K
cvng = 1:K
cvg = 1:K
for (j in 1:K){
  # split train data and make xgb.DMatrix
  train_data   <- data_variables[folds!=j,]
  train_label  <- data_label[folds!=j]
  train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
  # split test data and make xgb.DMatrix
  test_data  <- data_variables[folds==j,]
  test_label <- data_label[folds==j]
  test_matrix <- xgb.DMatrix(data = test_data, label = test_label)
  
  
  
  
  bst_model <- xgb.train(params = xgb_params,
                         data = train_matrix,
                         nrounds = nround)
  
  # Predict hold-out test set
  test_pred <- predict(bst_model, newdata = test_matrix)
  test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                            ncol=length(test_pred)/numberOfClasses) %>%
    t() %>%
    data.frame() %>%
    mutate(label = test_label + 1,
           max_prob = max.col(., "last"))
  # confusion matrix of test set
  predprob = rep(2,dim(test_data)[1])
  predprob[test_prediction[,1]>pp] = 1
  tb = table(test_prediction$label,predprob)
  cv.errors[j] = mean(test_prediction$label==predprob)
  cvng[j]  = tb[1,1]/sum(tb[1,])
  cvg[j]  = tb[2,2]/sum(tb[2,])
  tblist[[j]] = tb 
}
mean(cv.errors)
mean(cvng)
mean(cvg)
tblist

cfsum = tblist[[1]]
for (j in 2:K){
  cfsum = cfsum + tblist[[j]]
}
cfsum


# get the feature real names
names <-  colnames(dat[,-ncol(dat)])
# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix)


gp = xgb.ggplot.importance(importance_matrix)
print(gp) 

