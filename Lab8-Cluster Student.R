# Library -----------------------------------------
library(datasets)
library(tictoc)
library(ISLR2)
library(devtools)
# devtools::install_github("nwiroonsri/NCvalid",force=TRUE)
library(NCvalid)

# Important  Note ------------------------------ 
# - remove CH and GD33 Index
# - use STR, SF instead


# Iris Dataset --------------------------------------
iris
head(iris)
dim(iris)
Data_norm = scale(iris[,1:4])

## Kmeans clustering -----------------------
km.out2 = kmeans(Data_norm,2,nstart = 100)
km.out3 = kmeans(Data_norm,3,nstart = 100)

# par(mfrow=c(1,3))
# plot(pr.out$x[,1:2],col = km.out2$cluster+1 ,pch=20, cex=1,ann = FALSE,xaxt='n',yaxt='n')
# plot(pr.out$x[,1:2],col = realcol, pch=20, cex=1,ann = FALSE,xaxt='n',yaxt='n')
# plot(pr.out$x[,1:2],col = km.out3$cluster+1, pch=20, cex=1,ann = FALSE,xaxt='n',yaxt='n')

km.out3$cluster
iris[,5]
chkiris = 1:dim(iris)[1]
chkiris[km.out3$cluster==1] = 'setosa'
chkiris[km.out3$cluster==2] = 'versicolor'
chkiris[km.out3$cluster==3] = 'virginica'
mean(chkiris == iris[,5])


### Elbow -----------------------------------
par(mfrow=c(1,2))
y=1:7
for (k in 1:7){
  km.out=kmeans(Data_norm,k,nstart =100)
  y[k] = km.out$tot.withinss
}
plot(1:7,y,type="b",xlab="K",ylab = "Total Within-cluster distance")
plot(2:7,y[2:7],type="b",xlab="K",ylab = "Total Within-cluster distance")

### Index ---------------------------------
nc = Ovalid(Data_norm, kmax=10, kmin=2, method = 'kmeans', indexlist = ilist)
par(mar = c(4, 4, 0.5, 0.5))
par(mfrow=c(2,4))
for (i in 1:8){
  plot(data.frame(nc[i]), ylab = ilist2[i], xlab = "number of clusters",type='b')
  points(data.frame(nc[i])[which.max(data.frame(nc[i])[,2]),1],max(data.frame(nc[i])[,2]),col='red',pch=20)
}

km.out3$centers
head(iris)
colMeans(iris[km.out3$cluster==1,1:4])
colMeans(iris[km.out3$cluster==2,1:4])
colMeans(iris[km.out3$cluster==3,1:4])


# Simulated dataset -------------------------------
## Create dataset --------------------------
set.seed(100)
a = runif(550,min = 0,max = 2*pi)
r = runif(550,min = 0, max = 1)
x = r*cos(a)
y = r*sin(a)
dat = cbind(x,y)
dat[1:150,1] = dat[1:150,1]-5
dat[151:300,1] = dat[151:300,1]+5
dat[301:350,1] = dat[301:350,1]-3.5
dat[351:400,1] = dat[351:400,1]+3.5

par(mfrow=c(1,1))

realcol = c(rep(1,150),rep(2,150),rep(3,50),rep(4,50),rep(5,150))
plot(dat,col=realcol)
graphics.off()

## Kmeans cluserting --------------------------
km.out5 = kmeans(dat,5,nstart = 100)
km.out3 = kmeans(dat,3,nstart = 100)

par(mfrow=c(1,3))
plot(dat,col = km.out3$cluster+1 ,pch=20, cex=1,ann = FALSE,xaxt='n',yaxt='n')
plot(dat,col = realcol, pch=20, cex=1,ann = FALSE,xaxt='n',yaxt='n')
plot(dat,col = km.out5$cluster+1, pch=20, cex=1,ann = FALSE,xaxt='n',yaxt='n')

### Index ---------------------
nc = Ovalid(dat, kmax=10, kmin=2, method = 'kmeans', nstart=1000, indexlist = ilist)

# Plot the indices in the list
par(mar = c(4, 4, 0.5, 0.5))
par(mfrow=c(2,4))
for (i in 1:8){
  plot(data.frame(nc[i]), ylab = ilist2[i], xlab = "number of clusters",type='b')
  points(data.frame(nc[i])[which.max(data.frame(nc[i])[,2]),1],max(data.frame(nc[i])[,2]),col='red',pch=20)
}


# purchaseb Dataset ----------------------------------------------
phist = read.csv("purchaseb.csv",header=T,na.strings="?")
phist[,1]
rownames(phist) <- phist[,1]
phist = phist[,-1]
head(phist)
dim(phist)
phist = phist[rowSums(phist)!=0,]

## Hierarchical Clustering with Euclidean ------------------
?hclust
hc.complete =hclust(dist(phist), method="complete")
hc.average=hclust(dist(phist), method="average")
hc.single=hclust(dist(phist), method="single")
par(mfrow=c(1,3))
plot(hc.complete ,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average ,main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single ,main="Single Linkage", xlab="", sub="", cex=.9)

sum(cutree(hc.complete, 2)==1)
sum(cutree(hc.complete, 2)==2)

sum(cutree(hc.complete, 3)==1)
sum(cutree(hc.complete, 3)==2)
sum(cutree(hc.complete, 3)==3)

sum(colMeans(phist[cutree(hc.complete, 3)==1,]) )
sum(colMeans(phist[cutree(hc.complete, 3)==2,]) )
sum(colMeans(phist[cutree(hc.complete, 3)==3,]) )

phistg = cbind(cutree(hc.complete, 3),phist)
?write.csv
write.csv(phistg,file="pbtggroup.csv")

xsc=scale(phist)
hc.complete2 =hclust(dist(xsc), method="complete")
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features ")
sum(cutree(hc.complete2, 3)==1)
sum(cutree(hc.complete2, 3)==2)
sum(cutree(hc.complete2, 3)==3)

colMeans(phist[cutree(hc.complete2, 3)==1,]) 
colMeans(phist[cutree(hc.complete2, 3)==2,]) 
colMeans(phist[cutree(hc.complete2, 3)==3,]) 

par(mfrow=c(1,1))
y=rep(0,10)
for (k in 1:10)
{
  for (i in 1:k)
  {
    y[k]=y[k]+sum(dist(xsc[cutree(hc.complete2, k)==i,])^2)/sum(cutree(hc.complete2, k)==i)
  }
}
plot(1:10,y,type="b",xlab="K",ylab = "Total Within-cluster distance")


## Hierarchical Clustering with correlation -------------------
as.dist(1-cor(t(phist)))

hc.complete =hclust(as.dist(1-cor(t(phist))), method="complete")
hc.average=hclust(as.dist(1-cor(t(phist))), method="average")
hc.single=hclust(as.dist(1-cor(t(phist))), method="single")
par(mfrow=c(1,3))
plot(hc.complete ,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average ,main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single ,main="Single Linkage", xlab="", sub="", cex=.9)

sum(cutree(hc.complete, 2)==1)
sum(cutree(hc.complete, 2)==2)

sum(cutree(hc.complete, 3)==1)
sum(cutree(hc.complete, 3)==2)
sum(cutree(hc.complete, 3)==3)

sum(cutree(hc.average, 3)==1)
sum(cutree(hc.average, 3)==2)
sum(cutree(hc.average, 3)==3)

colMeans(phist[cutree(hc.complete, 3)==1,]) 
colMeans(phist[cutree(hc.complete, 3)==2,]) 
colMeans(phist[cutree(hc.complete, 3)==3,]) 

par(mfrow=c(1,1))
y=rep(0,10)
for (k in 1:10)
{
  for (i in 1:k)
  {
    y[k]=y[k]+sum(as.dist(1-cor(t(phist[cutree(hc.complete, k)==i,]))))/sum(cutree(hc.complete, k)==i)
  }
}
plot(1:10,y,type="b",xlab="K",ylab = "Total Within-cluster distance")

sum(cutree(hc.average, 2)==1)
sum(cutree(hc.average, 2)==2)

for (k in 1:8){
  print(sum(cutree(hc.average, 8)==k))
}

sales = NULL
for (k in 1:8){
  sales = rbind(sales,colMeans(phist[cutree(hc.complete, 8)==k,]))
}
sales
dim(sales)
write.csv(sales,file = "sales.csv")


# Cancer data -----------------------------------
nci.labs = NCI60$labs
nci.dat = NCI60$data
dim(nci.dat)
sd.data = scale(nci.dat)

par(mfrow = c(1, 3))
data.dist = dist(sd.data)
plot(hclust(data.dist), xlab = "", sub = "", ylab = "", labels = nci.labs , main = "Complete
Linkage")
plot(hclust(data.dist , method = "average"), labels = nci.labs , main = "Average
Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist , method = "single"), labels = nci.labs , main = "Single
Linkage", xlab = "", sub = "", ylab = "")

hc.out = hclust(dist(sd.data))
hc.clusters = cutree(hc.out , 4)
sum(hc.clusters==1)
sum(hc.clusters==2)
sum(hc.clusters==3)
sum(hc.clusters==4)
table(hc.clusters , nci.labs)


par(mfrow=c(1,1))
y=rep(0,10)
for (k in 1:10){
  for (i in 1:k){
    y[k]=y[k]+sum(dist(sd.data[cutree(hc.out, k)==i,])^2)/sum(cutree(hc.complete, k)==i)
  }
}
plot(1:10,y,type="b",xlab="K",ylab = "Total Within-cluster distance")

ilist =  c('NCI','NCI2', 'PB','CH','DI','SC','GD33','DB')
ilist2 =  c('NCI(L)','NCI2(L)', 'PB(L)','CH(L)','DI(L)','SC(L)','GD33(L)','DB(S)')
nc = Ovalid(sd.data, kmax=10, kmin=2, method = 'hclust_complete',indexlist = ilist)

par(mar = c(4, 4, 0.5, 0.5))
par(mfrow=c(2,4))
for (i in 1:7){
  plot(data.frame(nc[i]), ylab = ilist2[i], xlab = "number of clusters",type='b')
  points(data.frame(nc[i])[which.max(data.frame(nc[i])[,2]),1],max(data.frame(nc[i])[,2]),col='red',pch=20)
}

plot(data.frame(nc[8]), ylab = ilist2[i], xlab = "number of clusters",type='b')
points(data.frame(nc[8])[which.min(data.frame(nc[8])[,2]),1],min(data.frame(nc[8])[,2]),col='red',pch=20)




# LAB: Define Index -------------------------------
ilist =  c('NCI','NCI2', 'PB', 'STR', 'DI','SC', 'SF', 'DB')
ilist2 =  c('NCI(L)','NCI2(L)', 'PB(L)','STR(L)','DI(L)','SC(L)', 'SF(L)', 'DB(S)')

# LAB: Function ---------------------------------------------
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


# LAB: cure-t0-2000n-2D Data -------------------------------
data_cure = read.csv("cure-t0-2000n-2D.csv",header=T,na.strings="?")
dim(data_cure)
par(mfrow=c(1,1))
plot(data_cure[, 1:2])
# There are 3 groups from this graph.


## Kmeans clustering --------------------------
km_cure3 = kmeans(data_cure, 3, nstart = 50)
km_cure5 = kmeans(data_cure, 5, nstart = 50)

nc_cure = Ovalid(data_cure, kmax=10, kmin=2, method = 'kmeans', indexlist = ilist)
measure_index(nc_cure, "Data cure: K-Means")

# There are several result. However, DB index give that no. of optimal groups are 3. 


## Hierarchical Clustering with correlation ---------------------------
dissim <- as.dist(1-cor(t(data_cure)))
hc_cure_complete = hclust(dissim, method="complete")
hc_cure_average = hclust(dissim, method="average")
hc_cure_single = hclust(dissim, method="single")

par(mfrow=c(1,3))
plot(hc_cure_complete, main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc_cure_average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc_cure_single, main="Single Linkage", xlab="", sub="", cex=.9)

### Elbow -----------------------------
elbow(data_cure, hc_cure_complete, "Data cure: Hierarchical complete linkage")
elbow(data_cure, hc_cure_average, "Data cure: Hierarchical average linkage")
elbow(data_cure, hc_cure_single, "Data cure: Hierarchical single linkage")
#No. of optimal groups are 2.

### Index ---------------------------
nc_cure_comp = Ovalid(data_cure, kmax=10, kmin=2, method = 'hclust_complete', indexlist=ilist)
nc_cure_avg = Ovalid(data_cure, kmax=10, kmin=2, method = 'hclust_average', indexlist=ilist)
nc_cure_single = Ovalid(data_cure, kmax=10, kmin=2, method = 'hclust_single', indexlist=ilist)
measure_index(nc_cure_comp, "Data cure: Hierarchical complete linkage")
measure_index(nc_cure_avg, "Data cure: Hierarchical average linkage")
measure_index(nc_cure_single, "Data cure: Hierarchical single linkage")
# Using single linkage can get no. of optimal  groups that are 3 in NCI, NCI2, PB, STR, DI, and SC.  

# LAB: wine Data ------------------------------------------
data_wine = read.csv("wine.csv",header=T,na.strings="?")
dim(data_wine)
summary(data_wine)
# There are 3 classes of wine.

## Kmeans clustering ------------------------
nc_wine = Ovalid(data_wine, kmax=10, kmin=2, method = 'kmeans', indexlist = ilist)
measure_index(nc_wine, "Data wine: K-Means")
# There are several result, and no correct answer.

## Hierarchical Clustering with correlation ---------------------------
dissim_wine <- as.dist(1-cor(t(data_wine)))
hc_wine_complete = hclust(dissim_wine, method="complete")
hc_wine_average = hclust(dissim_wine, method="average")
hc_wine_single = hclust(dissim_wine, method="single")

par(mfrow=c(1,3))
plot(hc_wine_complete, main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc_wine_average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc_wine_single, main="Single Linkage", xlab="", sub="", cex=.9)

### Elbow -----------------------------
elbow(data_wine, hc_wine_complete, "Data wine: Hierarchical complete linkage")
elbow(data_wine, hc_wine_average, "Data wine: Hierarchical average linkage")
elbow(data_wine, hc_wine_single, "Data wine: Hierarchical single linkage")
#From elbow, no. of optimal groups are 2.

### Index ---------------------------
nc_wine_comp = Ovalid(data_wine, kmax=10, kmin=2, method = 'hclust_complete', indexlist=ilist)
nc_wine_avg = Ovalid(data_wine, kmax=10, kmin=2, method = 'hclust_average', indexlist=ilist)
nc_wine_single = Ovalid(data_wine, kmax=10, kmin=2, method = 'hclust_single', indexlist=ilist)
measure_index(nc_wine_comp, "Data wine: Hierarchical complete linkage")
measure_index(nc_wine_avg, "Data wine: Hierarchical average linkage")
measure_index(nc_wine_single, "Data wine: Hierarchical single linkage")
# The linkage and index that give no. of groups is 3 are following:
#   1, Single linkage with DB index
#   2. Average linkage with DI index
