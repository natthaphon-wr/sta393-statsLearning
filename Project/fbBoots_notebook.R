# Import Library ---------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(cluster)
library(readr)
library(ggplot2)

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

