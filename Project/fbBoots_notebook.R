# Import Library ---------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(cluster)
library(readr)
library(ggplot2)
library(cluster)
library(fpc)

# Data Preparation -------------------------------------------------------------
fbBootDB <- read_csv('footballbootsdb.csv')
summary(fbBootDB)
colSums(is.na(fbBootDB))

## Columns description ------------------------------------------
# 1. RecordID:  The unique identifier for each record in the dataset.
# 2. ClubName:  The name of the club that the player who wore the football boots belongs to.
# 3. League/Country:  League: The league in which the football boots were used. Country: The country in which the league is played.
# 4. BootsName: The name of the football boot model.
# 5. BootsMaterial: The material used for the upper part of the football boot.
# 6. BootsBrand:  The brand of the football boot.
# 7. BootsPack: The name of the pack or collection that the football boot belongs to.
# 8. BootsType: The type of football boot
# 9. BootsPosition: The position of the player that the football boot is designed for.
# 10. BootsTopPlayers:  he top player associated with the football boot model.
# 11. PlayerName: The name of the player who wore the football boots.
# 12. PlayerPosition: The position of the player on the pitch.
# 13. PlayerNationality: The nationality of the player who wore the football boots.
# 14. PlayerMarketValue: The market value of the player who wore the football boots. (million euro)

## Delete 4 last columns ----------------------------------------
prep_data <- fbBootDB[,1:14]
colSums(is.na(prep_data))
sapply(prep_data, function(x) n_distinct(x))
summary(prep_data)

## Find all `null` value manually -------------------------------
# null boots
nullBoots<- prep_data[prep_data$BootsName=='null',]
prep_data <- prep_data[!prep_data$BootsName=='null',]
# There are still some columns that are null, but it's understandable that
#   there aren't information for those boots or they haven't specific function.


## Define Type of Data -----------------------------------------------
summary(prep_data)
sapply(prep_data, function(x) n_distinct(x))
# 1. Character (not use in model): RecordID, PlayerName
# 2. Categorical: ClubName, League/Country, BootsName, BootsMaterial, BootsBrand, 
#                 BootsPack, BootsType, BootsPosition, PlayerPosition, PlayerNationality
# 3. Numerical:   PlayerMarkerValue

### Convert to numerical variables ----------------------------
prep_data$PlayerMarketValue <- gsub("million", "", prep_data$PlayerMarketValue)
prep_data$PlayerMarketValue  <- gsub("€", "", prep_data$PlayerMarketValue)
prep_data <- prep_data %>% mutate_at(vars("PlayerMarketValue"), as.numeric)

### Convert to categorical variables ----------------------------
prep_data <- prep_data %>% mutate_at(vars(-"RecordID", -"PlayerName", -"PlayerMarketValue"), as.factor)

### Reorder PlayerName column ------------------------------------
prep_data <- prep_data %>% relocate(PlayerName, .after = RecordID)

## Conclude data preparation -------------------------------------
summary(prep_data)
sapply(prep_data, function(x) n_distinct(x))


# Data Exploration -------------------------------------------------------------
sapply(prep_data, function(x) n_distinct(x))

## League/Country ----------------------------------
table(prep_data$`League/Country`)
# There is a issue that there are "Champions League" and "Europa League", 
#   while most data are actually league. This problem can't clean more. 
#   Must noted before using.

## BootsBrand ----------------------------------
table(prep_data$BootsBrand)
barplot(sort(table(prep_data$BootsBrand), decreasing=TRUE), 
        main="Boots Brand",
        xlab="Brand",
        ylab="Count")

## BootsType ----------------------------------
table(prep_data$BootsType)
barplot(sort(table(prep_data$BootsType), decreasing=TRUE), 
        main="Boots Type",
        xlab="Boots Type",
        ylab="Count")

## BootsPosition -----------------------------
table(prep_data$BootsPosition)
barplot(sort(table(prep_data$BootsPosition), decreasing=TRUE), 
        main="Boots Position",
        xlab="Boots Position",
        ylab="Count")

## PlayerPosition ---------------------------
table(prep_data$PlayerPosition)
barplot(sort(table(prep_data$PlayerPosition), decreasing=TRUE), 
        main="Player Position",
        xlab="Player Position",
        ylab="Count")

## PlayerMarketValue -----------------------
hist(prep_data$PlayerMarketValue,
     main="Histogram of Player Market Value",
     xlab="Market Value (Million Euro)")


# Clustering -------------------------------------------------------------------

## Dissimilarity Matrix -------------------------------------
# using Gower distance for both categorical and numerical data
gower_dist <- daisy(prep_data[,3:14], metric = c("gower"))

## hclust ---------------------------------------------------
hc_complete = hclust(gower_dist, method="complete")
plot(hc_complete, main = "Agglomerative, complete linkages")
hc_single = hclust(gower_dist, method="single")
plot(hc_single, main = "Agglomerative, single linkages")
hc_average = hclust(gower_dist, method="average")
plot(hc_average, main = "Agglomerative, average linkages")
# Single linkage is very unbalanced. 
# Complete and average are quite balanced in the same level

## No. of cluster Measurement -------------------------------
# I think the suitable no. of cluster are 4-7. 
# (more than 3 because there are 3 large brands)

# This function from https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995 
cstats.table <- function(dist, tree, k) {
  clust.assess <- c("cluster.number","n","within.cluster.ss","average.within",
                    "average.between","wb.ratio","dunn2","avg.silwidth")
  clust.size <- c("cluster.size")
  stats.names <- c()
  row.clust <- c()
  output.stats <- matrix(ncol = k, nrow = length(clust.assess))
  cluster.sizes <- matrix(ncol = k, nrow = k)
  for(i in c(1:k)){
    row.clust[i] <- paste("Cluster-", i, " size")
  }
  for(i in c(2:k)){
    stats.names[i] <- paste("Test", i-1)
    for(j in seq_along(clust.assess)){
      output.stats[j, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])[j]
    }
    for(d in 1:k) {
      cluster.sizes[d, i] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.size])[d]
      dim(cluster.sizes[d, i]) <- c(length(cluster.sizes[i]), 1)
      cluster.sizes[d, i]
    }
  }
  output.stats.df <- data.frame(output.stats)
  cluster.sizes <- data.frame(cluster.sizes)
  cluster.sizes[is.na(cluster.sizes)] <- 0
  rows.all <- c(clust.assess, row.clust)
  # rownames(output.stats.df) <- clust.assess
  output <- rbind(output.stats.df, cluster.sizes)[ ,-1]
  colnames(output) <- stats.names[2:k]
  rownames(output) <- rows.all
  is.num <- sapply(output, is.numeric)
  output[is.num] <- lapply(output[is.num], round, 2)
  return(output)
}

stats_hc_c <- cstats.table(gower_dist, hc_complete, 7)
stats_hc_c
stats_hc_avg <- cstats.table(gower_dist, hc_average, 7)
stats_hc_avg
# See that complete linkage is more balance

### Elbow ------------------------------------------------
ggplot(data = data.frame(t(cstats.table(gower_dist, hc_complete, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Agglomerative clustering") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))



# Other Analysis ---------------------------------------------------------------
# 1. German player/league use German Brand (Adidas and Puma) most?
# 2. Relationship b/w  BootsType and PlayerPosition
#    Example: Speed Boots for Wing/Attacker player?, Control for Midfield?
# 3. Relationship b/w BootsBrand and BootsType: to see that 
#    "Are Bootsbrands produced BootsType as Brand's character?"
# 4. Relationship b/w BootsBrand and PlayerMarket
#    "3 Large brands (NIKE, ADIDAS, PUMA) have different top players as partner"

