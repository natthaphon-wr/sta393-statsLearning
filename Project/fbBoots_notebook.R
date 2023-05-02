# Import Library ---------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(cluster)
library(readr)
library(ggplot2)
library(cluster)
library(fpc)
library(reshape2)
library(purrr)
library(dendextend)
library(viridis)

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

## NA value in BootsBrand ---------------------------------
prep_data[is.na(prep_data$BootsBrand),]
# remove this row
prep_data <- prep_data[!is.na(prep_data$BootsBrand),]

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
colSums(is.na(prep_data))


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
        las=2,
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
# I think the suitable no. of cluster are 4-10. 
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

### Within clusters SS --------------------------------------------
ggplot(data = data.frame(t(cstats.table(gower_dist, hc_complete, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Clustering (Complete Linkage)") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(t(cstats.table(gower_dist, hc_average, 15))), 
       aes(x=cluster.number, y=within.cluster.ss)) + 
  geom_point()+
  geom_line()+
  ggtitle("Clustering (Average Linkage)") +
  labs(x = "Num.of clusters", y = "Within clusters sum of squares (SS)") +
  theme(plot.title = element_text(hjust = 0.5))


### Silhouette Width --------------------------------------------
ggplot(data = data.frame(t(cstats.table(gower_dist, hc_complete, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Clustering (Complete Linkage)") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = data.frame(t(cstats.table(gower_dist, hc_average, 15))), 
       aes(x=cluster.number, y=avg.silwidth)) + 
  geom_point()+
  geom_line()+
  ggtitle("Clustering (Average Linkage)") +
  labs(x = "Num.of clusters", y = "Average silhouette width") +
  theme(plot.title = element_text(hjust = 0.5))

### Selection --------------------------------------------------
# From complete linkage graphs, the optimal number is 6 or 12.
# From average linkage graphs, the optimal number is 4 or 14.
# However, I think 4 groups are quite small, and 12/14 are too much.
# So, I will use complete linkage with no. of group is 6.

stats_hc_c <- cstats.table(gower_dist, hc_complete, 10)
stats_hc_c
# From this statistic in Test5 (n=6), the balance is acceptable.


## Visualize Result --------------------------------------------------------
cut_best <- cutree(hc_complete, 6)

### Dendrogram ----------------------------------------
dendro_comp <- as.dendrogram(hc_complete)
comp_col_dend <- color_branches(dendro_comp, k=6)
plot(comp_col_dend, main="Dendrogram, k = 6")

### Dendrogram2 ----------------------------------------
# dendro <- as.dendrogram(hc_complete)
# dendro.col <- dendro %>%
#   set("branches_k_color", k=6) %>%
#   set("branches_lwd", 0.6) %>%
#   set("labels_colors", value = c("darkslategray")) %>%
#   set("labels_cex", 0.5)
# ggd1 <- as.ggdend(dendro.col)
# ggplot(ggd1, theme=theme_minimal()) +
#   labs(x="Number of Observations", y="Height", title="Dendrogram, k = 6")

### Radical Plot -----------------------------------------
# ggplot(ggd1, labels=T) + 
#   scale_y_reverse(expand = c(0.2, 0)) +
#   coord_polar(theta="x")

### Heatmap -----------------------------------------------
# clust.num <- cutree(hc_complete, k=6)
prepData.cl <- cbind(prep_data, cut_best)
cust.long <- melt(data.frame(lapply(prepData.cl, as.character), stringsAsFactors=FALSE),
                  id = c("RecordID", "cut_best"), factorsAsStrings=T)
cust.long.q <- cust.long %>%
  group_by(cut_best, variable, value) %>%
  mutate(count = n_distinct(RecordID)) %>%
  distinct(cut_best, variable, value, count)
cust.long.p <- cust.long.q %>%
  group_by(cut_best, variable) %>%
  mutate(perc = count / sum(count)) %>%
  arrange(cut_best)
heatmap.p <- ggplot(cust.long.p, aes(x = cut_best, y = factor(value, ordered = T))) +
  geom_tile(aes(fill = perc), alpha = 0.85) +
  labs(title = "Distribution of characteristics across clusters", x = "Cluster number", y = NULL) +
  geom_hline(yintercept = 3.5) +
  geom_hline(yintercept = 10.5) +
  geom_hline(yintercept = 13.5) +
  geom_hline(yintercept = 17.5) +
  geom_hline(yintercept = 21.5) +
  scale_fill_gradient2(low="chartreuse1", mid="burlywood1", high="cadetblue1")
heatmap.p


### Plot each variable --------------------------------
# Explore important variables.
Boots_cluster <- mutate(prep_data, cluster=cut_best)
count(Boots_cluster, cluster)
Boots_cluster$cluster <- as.factor(Boots_cluster$cluster)
summary(Boots_cluster)
# colSums(is.na(Boots_cluster))

ggplot(Boots_cluster, aes(x=1:dim(Boots_cluster)[1], y=PlayerMarketValue, color=cluster)) + 
  geom_point() +
  xlab("Data") +
  ggtitle("Boots Data with Clustering")

table(Boots_cluster$BootsPosition, Boots_cluster$cluster)
barplot(table(Boots_cluster$BootsPosition, Boots_cluster$cluster),
        col = viridis(4),
        ylim = c(0, 2000),
        beside = TRUE,
        legend.text = TRUE,
        main = "BootsPosition with Clustering",
        xlab ="Cluster",
        ylab = "Count")

table(Boots_cluster$BootsType, Boots_cluster$cluster)
barplot(table(Boots_cluster$BootsType, Boots_cluster$cluster),
        col = viridis(9),
        ylim = c(0, 2000),
        beside = TRUE,
        legend.text = TRUE,
        args.legend = list("topright", cex=0.75),
        main = "BootsType with Clustering",
        xlab = "Cluster",
        ylab = "Count")

table(Boots_cluster$BootsBrand, Boots_cluster$cluster)
barplot(table(Boots_cluster$BootsBrand, Boots_cluster$cluster),
        col = viridis(13),
        ylim = c(0, 1200),
        beside = TRUE,
        legend.text = TRUE,
        args.legend = list(x="topright", cex=0.5, inset=c(-0.02, -0.1)),
        main = "BootsBrand with Clustering",
        xlab = "Cluster",
        ylab = "Count")

table(Boots_cluster$PlayerPosition, Boots_cluster$cluster)
barplot(table(Boots_cluster$PlayerPosition, Boots_cluster$cluster),
        col = viridis(13),
        ylim = c(0, 400),
        beside = TRUE,
        legend.text = TRUE,
        args.legend = list(x="topright", cex=0.5),
        main = "PlayerPosition with Clustering",
        xlab = "Cluster",
        ylab = "Count")


# Other Analysis ---------------------------------------------------------------
# 1. German player/league use German Brand (Adidas and Puma) most?
# 2. Relationship b/w  BootsType and PlayerPosition
#    Example: Speed Boots for Wing/Attacker player?, Control for Midfield?
# 3. Relationship b/w BootsBrand and BootsType: to see that 
#    "Are Bootsbrands produced BootsType as Brand's character?"
# 4. Relationship b/w BootsBrand and PlayerMarket
#    "3 Large brands (NIKE, ADIDAS, PUMA) have different top players as partner"

## Analysis 1 -------------------------------------------------------
print(count(Boots_cluster, Boots_cluster$`League/Country`), n=26)
bundes_data <- Boots_cluster[Boots_cluster$`League/Country` == c("Bundesliga"),]

bundes_brand <- bundes_data %>% group_by(BootsBrand) %>% summarise(ratio=n()/dim(bundes_data)[1])
ggplot(data=bundes_brand, aes(x=reorder(BootsBrand, -ratio), y=ratio)) +
  geom_bar(stat="identity") +
  xlab("BootsBrand") +
  ylab("Ratio") +
  ggtitle("BootsBrand in Bundesliga") +
  geom_text(aes(label=round(ratio,2)), vjust = -0.2,)

total_brand <- Boots_cluster %>% group_by(BootsBrand) %>% summarise(ratio=n()/dim(Boots_cluster)[1])
ggplot(data=total_brand, aes(x=reorder(BootsBrand, -ratio), y=ratio)) +
  geom_bar(stat="identity") +
  xlab("BootsBrand") +
  ylab("Ratio") +
  ggtitle("BootsBrand in Total Data") +
  geom_text(aes(label=round(ratio,2)), vjust = -0.2,)

## Analysis 2 --------------------------------------------------------


