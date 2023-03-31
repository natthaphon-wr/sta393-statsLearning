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

## Pivot Table and Clean Data --------------------------------
pivot_period <- bread_basket %>%
  mutate(count = 1) %>% 
  distinct() %>% 
  pivot_wider(names_from="period_day", values_from="count", values_fill = 0)

pivot_weekday <- pivot_period %>%
  mutate(count = 1) %>% 
  distinct() %>% 
  pivot_wider(names_from="weekday_weekend", values_from="count", values_fill = 0)
summary(pivot_weekday)

pivot_data <- pivot_weekday %>%
  mutate(count = 1) %>% 
  distinct() %>% 
  pivot_wider(names_from="Item", values_from="count", values_fill = 0) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.numeric, as.logical) %>% 
  select(-c(date_time, Transaction))
summary(pivot_data)


## Convert to Transaction Data -----------------------------
bb_trans <- transactions(pivot_data)
summary(bb_trans)
# It shows that 9465 rows (transactions), and 100 columns (items).
# It's correct, 100 columns = 94 goods + 2 weekday_weekend + 4 period_day


# Explore Data ----------------------------------------------------------------
hist(size(bb_trans), main="Number of Items per Transaction", xlab="#Items")
mtext(paste("Total:", length(bb_trans), "Transactions,", sum(size(bb_trans)), "Items"))
# It show that most transaction have a little number of items. (less than 4)

itemFrequencyPlot(bb_trans, topN = 20, type='relative')
itemFrequencyPlot(bb_trans, topN = 20, type='absolute')
# It shows top20 most frequents items. There are some interesting points.
#   1. Transaction on weekday are more than weekend
#   2. Most transaction are on afternoon and morning respectively.
#   3. Most frequents product is Coffee, and the second is Bread. 
#      Both products have amount different frequency with others.
#   4. Frequency of Coffee is more than Morning and Weekend.
#   5. And other top items are food/snack/drink too such as Tea, Sandwich, Cake.


# Determine Association Rules -------------------------------------------------
rule_top <- apriori(bb_trans, parameter=list(minlen=1, maxlen=5, support=0.001, confidence=0.5))
rule_bottom <- apriori(bb_trans, parameter=list(minlen=1, maxlen=5, support=0.001, confidence=0))
l1 = length(rule_top)
l2 = length(rule_bottom)


# Inspect Top Support ----------------------------------------------------------
inspect(head(sort(rule_top, by ="support"), 20))


# Inspect Top Confidence -------------------------------------------------------
inspect(head(sort(rule_top, by ="confidence"), 20))


# Inspect Top Lift -------------------------------------------------------------
inspect(head(sort(rule_top, by ="lift"), 20))





