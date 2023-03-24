# Import Library -------------------
library(foreign)
library(NCvalid)
library(dplyr)
library(factoextra)
library(readr)

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
legend(x="topleft", legend=unique(artData$class), pch=1, col=as.vector(unique(artData$class)))
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
nc_artData = Ovalid(artData, kmax=10, kmin=2, method = 'kmeans', indexlist = ilist)
measure_index(nc_artData, "Artificial Data: K-Means")


# Wholesale Dataset ------------------------------------------
## Read and explore data ---------------------
# I will not scale data for considering price different between goods.
Wholesale <- read_csv("Wholesale.csv")
Wholesale <- Wholesale %>% select(-c('Channel', 'Region'))
summary(Wholesale)
# I will use hierarchical clustering. 

## Use Euclidean Distance -----------------------------------
disEuc <- dist(Wholesale)
hc_eucli_complete = hclust(disEuc, method="complete")
hc_eucli_average = hclust(disEuc, method="average")
hc_eucli_single = hclust(disEuc, method="single")

par(mfrow=c(1,3))
plot(hc_eucli_complete, main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc_eucli_average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc_eucli_single, main="Single Linkage", xlab="", sub="", cex=.9)

# Those trees are a lot unbalanced.


### Elbow -----------------------------
elbow(Wholesale, hc_eucli_complete, "Euclidean Distance: Hierarchical complete linkage")
elbow(Wholesale, hc_eucli_average, "Euclidean Distance: Hierarchical average linkage")
elbow(Wholesale, hc_eucli_single, "Euclidean Distance: Hierarchical single linkage")
#complete/avg/single = 6/10/6?
tab = table(cutree(hc_eucli_complete, 6))

table(cutree(hc_eucli_complete, 6))
table(cutree(hc_eucli_average, 10))
table(cutree(hc_eucli_single, 6))
# It's showed that there are many groups (almost entirely) that have 1-2 members. 

### Index ----------------------------
nc_eucli_comp = Ovalid(Wholesale, kmax=10, kmin=2, method = 'hclust_complete', indexlist=ilist)
nc_eucli_avg = Ovalid(Wholesale, kmax=10, kmin=2, method = 'hclust_average', indexlist=ilist)
nc_eucli_single = Ovalid(Wholesale, kmax=10, kmin=2, method = 'hclust_single', indexlist=ilist)
measure_index(nc_eucli_comp, "Euclidean Distance: Hierarchical complete linkage")
measure_index(nc_eucli_avg, "Euclidean Distance: Hierarchical average linkage")
measure_index(nc_eucli_single, "Euclidean Distance: Hierarchical single linkage")
# It's showed different answers in each index (2-9) and each linkage.

### Summary ------------------------------------
# When using no.of groups that are more than 6, there are many groups that have 1-2 members.
#   There is only 2-3 groups that have appropriate number (actually can call group).
# So, I will use n = 3,4,5
for (i in 3:5){
  print("complete linkage")
  print(table(cutree(hc_eucli_complete, i)))
  print("average linkage")
  print(table(cutree(hc_eucli_average, i)))
  print("single linkage")
  print(table(cutree(hc_eucli_single, i)))
}
#Found same problem too.


## Use Correlation Dissimilarity -----------------------------------
disCorr <- as.dist(1-cor(t(Wholesale)))
hc_corr_complete = hclust(disCorr, method="complete")
hc_corr_average = hclust(disCorr, method="average")
hc_corr_single = hclust(disCorr, method="single")

par(mfrow=c(1,3))
plot(hc_corr_complete, main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc_corr_average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc_corr_single, main="Single Linkage", xlab="", sub="", cex=.9)

#It's better than euclidean distance.




