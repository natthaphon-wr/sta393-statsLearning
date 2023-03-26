# Import Library -------------------
library(foreign)
library(NCvalid)
library(dplyr)
library(factoextra)
library(readr)
library(dendextend)
library(ggplot2)

# Define Index -------------------------------
ilist =  c('NCI','NCI2', 'PB', 'STR', 'DI','SC', 'SF', 'DB')
ilist2 =  c('NCI(L)','NCI2(L)', 'PB(L)','STR(L)','DI(L)','SC(L)', 'SF(L)', 'DB(S)')


# Function ------------------------
measure_index <- function(nc, title){
  par(mfrow=c(2,4))
  for (i in 1:8){
    plot(data.frame(nc[i]), ylab = ilist2[i], xlab = "number of clusters", type='b')
    points(data.frame(nc[i])[which.max(data.frame(nc[i])[,2]),1],max(data.frame(nc[i])[,2]), col='red', pch=20)
  }
  mtext(title, side=3, line=-2, outer=TRUE)
}

elbow <- function(data, hc, title){
  par(mfrow=c(1,1))
  y=rep(0,10)
  for (k in 1:10){
    for (i in 1:k){
      y[k] = y[k] + sum(as.dist(1-cor(t(data[cutree(hc, k)==i,]))))/sum(cutree(hc, k)==i)
    }
  }
  plot(1:10, y, type="b", xlab="K", ylab="Total Within-cluster distance", main = title)
}


# Artificial Data -----------------------------------------
## Read and explore data ---------------------
artData <- read.arff('2d-3c-no123.arff')
plot(artData$a0, artData$a1, col=artData$class)
summary(artData)

artData <- artData %>% mutate_at(c('class'), as.numeric)
summary(artData)
# There are 3 classes.
# From the graph above, I will use K-means clustering

## Kmeans clustering -----------------------
kms_artData = kmeans(artData[, -3], 3, nstart = 20)
kms_artData$cluster
fviz_cluster(kms_artData, data=artData[,-3], geom = "point")

### Using indexes for select no. of class ----------------
nc_artData = Ovalid(artData[, -3], kmax=10, kmin=2, method = 'kmeans', indexlist = ilist)
measure_index(nc_artData, "Artificial Data: K-Means")


# Wholesale Dataset ------------------------------------------
## Read and explore data ---------------------
Wholesale <- read_csv("Wholesale.csv")
Wholesale <- Wholesale %>% select(-c('Channel', 'Region'))
summary(Wholesale)

par(mfrow=c(2,3))
plot(1:dim(Wholesale)[1], Wholesale$Fresh, xlab="Data", ylab="Fresh")
plot(1:dim(Wholesale)[1], Wholesale$Milk, xlab="Data", ylab="Milk")
plot(1:dim(Wholesale)[1], Wholesale$Grocery, xlab="Data", ylab="Grocery")
plot(1:dim(Wholesale)[1], Wholesale$Frozen, xlab="Data", ylab="Frozen")
plot(1:dim(Wholesale)[1], Wholesale$Detergents_Paper, xlab="Data", ylab="Detergents_Paper")
plot(1:dim(Wholesale)[1], Wholesale$Delicassen, xlab="Data", ylab="Delicassen")

# I will use hierarchical clustering, and
#   will not scale data for considering price different between goods.

## Use Euclidean Distance -----------------------------------
disEuc <- dist(Wholesale)
hc_eucli_complete = hclust(disEuc, method="complete")
hc_eucli_average = hclust(disEuc, method="average")
hc_eucli_single = hclust(disEuc, method="single")

# Sometimes, "Session fatal error" happen when plot these graph.
par(mfrow=c(1,3))
plot(hc_eucli_complete, main="Complete Linkage", check = TRUE)
plot(hc_eucli_average, main="Average Linkage", check = TRUE)
plot(hc_eucli_single, main="Single Linkage", check = TRUE)

# Those trees are a lot unbalanced.


### Elbow -----------------------------
elbow(Wholesale, hc_eucli_complete, "Euclidean Distance: Hierarchical complete linkage")
elbow(Wholesale, hc_eucli_average, "Euclidean Distance: Hierarchical average linkage")
elbow(Wholesale, hc_eucli_single, "Euclidean Distance: Hierarchical single linkage")
#complete/avg/single = 6/10/6?
table(cutree(hc_eucli_complete, 6))
table(cutree(hc_eucli_average, 10))
table(cutree(hc_eucli_single, 6))
# It's showed that there are many groups (almost entirely) that have 1-2 members. 


## Use Correlation Dissimilarity -----------------------------------
disCorr <- as.dist(1-cor(t(Wholesale)))
hc_corr_complete = hclust(disCorr, method="complete")
hc_corr_average = hclust(disCorr, method="average")
hc_corr_single = hclust(disCorr, method="single")

# Sometimes, "Session fatal error" happen when plot these graph.
par(mfrow=c(1,3))
plot(hc_corr_complete, main="Complete Linkage", check = TRUE)
plot(hc_corr_average, main="Average Linkage", check = TRUE)
plot(hc_corr_single, main="Single Linkage", check = TRUE)
#It's more balance than euclidean distance.

### Elbow -----------------------------
elbow(Wholesale, hc_corr_complete, "Correlation Dissimilarity: Hierarchical complete linkage")
elbow(Wholesale, hc_corr_average, "Correlation Dissimilarity: Hierarchical average linkage")
elbow(Wholesale, hc_corr_single, "Correlation Dissimilarity: Hierarchical single linkage")
#complete/avg/single = 3/3/8?
table(cutree(hc_corr_complete, 3))
table(cutree(hc_corr_average, 3))
table(cutree(hc_corr_single, 8))
# From complete and average linkage that number of group are 3, there are 2 large groups and
#   a small group. (average has 1 member for a small group)


# Summary -------------------------------------------
# From Elbow method for selecting optimal number of cluster with using 
#   distance and linkage, the most optimal/reasonable that I think is 
#   the model using correlation dissimilarity with complete linkage,
#   and number of cluster are 3.
# Explore the result of this model.

cut_best <- cutree(hc_corr_complete, 3)
plot(hc_corr_complete, main="Complete Linkage", check = TRUE)
rect.hclust(hc_corr_complete , k = 3, border = 2:6)
abline(h = 3, col = 'red')

corrComp_dend_obj <- as.dendrogram(hc_corr_complete)
comp_col_dend <- color_branches(corrComp_dend_obj, h=1.5)
plot(comp_col_dend, main="Best Model with Cuttree")

Wholesale_cluster <- mutate(Wholesale, cluster = cut_best)
count(Wholesale_cluster, cluster)
Wholesale_cluster$cluster <- as.factor(Wholesale_cluster$cluster)
summary(Wholesale_cluster)

ggplot(Wholesale_cluster, aes(x=1:dim(Wholesale_cluster)[1], y=Fresh, color=cluster)) + 
  geom_point() +
  xlab("Data") +
  ggtitle("Wholesale Data with Clustering from Model")
ggplot(Wholesale_cluster, aes(x=1:dim(Wholesale_cluster)[1], y=Milk, color=cluster)) + 
  geom_point() +
  xlab("Data") +
  ggtitle("Wholesale Data with Clustering from Model")
ggplot(Wholesale_cluster, aes(x=1:dim(Wholesale_cluster)[1], y=Grocery, color=cluster)) + 
  geom_point() +
  xlab("Data") +
  ggtitle("Wholesale Data with Clustering from Model")
ggplot(Wholesale_cluster, aes(x=1:dim(Wholesale_cluster)[1], y=Frozen, color=cluster)) + 
  geom_point() +
  xlab("Data") +
  ggtitle("Wholesale Data with Clustering from Model")
ggplot(Wholesale_cluster, aes(x=1:dim(Wholesale_cluster)[1], y=Detergents_Paper, color=cluster)) + 
  geom_point() +
  xlab("Data") +
  ggtitle("Wholesale Data with Clustering from Model")
ggplot(Wholesale_cluster, aes(x=1:dim(Wholesale_cluster)[1], y=Delicassen, color=cluster)) + 
  geom_point() +
  xlab("Data") +
  ggtitle("Wholesale Data with Clustering from Model")

