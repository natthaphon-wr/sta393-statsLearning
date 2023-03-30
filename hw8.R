# HW8: Association Rules

# Import Library --------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(arules)

# Data Preparation ------------------------------------------------------------
bread_basket <- read_csv("bread+basket.csv")
summary(bread_basket)
n_trans <- length(unique(bread_basket$Transaction))
# There are 9465 transaction.
# Association rule isn't consider quantity in each items.

pivot_data <- bread_basket %>%
  mutate(count = 1) %>% 
  distinct() %>% 
  pivot_wider(names_from="Item", values_from="count", values_fill = 0)

sum(is.nan(as.matrix(pivot_data)))
# Data is already for using. (no NaN)

# Explore Data ----------------------------------------------------------------
## Count number of item in each transaction ----------------------
pivot_data[,ncol(pivot_data)]
pivot_data_count <- pivot_data %>% 
  mutate(nitems = rowSums(across(Bread:`Tacos/Fajita`)))
pivot_data_count$nitems
hist(pivot_data_count$nitems, main="Number of Items per Transaction", xlab="#Items")

## Frequent Items -----------------------------------------------
# transactions <- as(pivot_data, "transactions")
# item_frequencies <- itemFrequency(transactions, type="a")
# itemFrequencyPlot(item_frequencies, topN = 25)

