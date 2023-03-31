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
pivot_data <- bread_basket %>%
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
itemFrequencyPlot(bb_trans, topN = 20)

