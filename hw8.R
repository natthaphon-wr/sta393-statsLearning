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

itemFrequencyPlot(bb_trans, topN = 30, type='relative')
itemFrequencyPlot(bb_trans, topN = 30, type='absolute')
# It shows top20 most frequents items. There are some interesting points.
#   1. Transaction on weekday are more than weekend
#   2. Most transaction are on afternoon and morning respectively.
#   3. Most frequents product is Coffee, and the second is Bread. 
#      Both products have amount different frequency with others.
#   4. Frequency of Coffee is more than Morning and Weekend.
#   5. And other top items are food/snack/drink too such as Tea, Sandwich, Cake.


# Determine Association Rules -------------------------------------------------
rule1 <- apriori(bb_trans, parameter=list(minlen=1, maxlen=5, support=0.001, confidence=0.5))
rule2 <- apriori(bb_trans, parameter=list(minlen=1, maxlen=5, support=0.01, confidence=0.5))
rule3 <- apriori(bb_trans, parameter=list(minlen=1, maxlen=5, support=0.001, confidence=0))
l1 = length(rule1)
l2 = length(rule2)
l3 = length(rule3)

# Top Support ----------------------------------------------------------
inspect(head(sort(rule1, by ="support"), 50))
# From this inspect by support, the interesting points are following:
#   1. Afternoon at weekday is the most frequent time.
#   2. Buying coffee frequent in morning/afternoon at weekday.
#   3. Bread, tea, cake, sandwich, pastry, cookies, Medialuna (croissants) 
#      are frequently products in morning/afternoon at weekday too. 
#      (order are respectively)
#   4. Bread, cake, pastry, Medialuna (croissants) are frequently paired with coffee.
# To summary this inspect, people usually buy coffee and snacks before working in 
#   morning/afternoon or during break at weekday.


# Top Confidence -------------------------------------------------------
## Rule1 ----------------------------------------------
inspect(head(sort(rule1, by ="confidence"), 50))
# From rule1 with minimum support is 0.001, it shows many of small cases that 
#   confidence = 1. It's tricky to interpret from this inspect.

## Rule2 ----------------------------------------------
inspect(head(sort(rule2, by ="confidence"), 50))
# The result isn't tricky, it's good to find some meaning of these. 
# From this inspect sorting by confidence, the interesting points are following:
#   1. Top 17 confidence are cases that afternoon is consequent.
#   2. People usually buy various food in afternoon at weekday such as coffee, 
#      soup, sandwich, chicken stew, and scone.
#   3. From 2., there are some products that in top frequent, and some aren't.
#      (from inspect by support)
#   4. When people buying toast in morning at weekday (or other combination),
#      they also buy coffee too.


# Top Lift ---------------------------------------------------------------------
## Rule1 ------------------------------------------------
inspect(head(sort(rule1, by ="lift"), 50))
# The result shows 7 outstanding cases that have high lift, that means antecedent and 
#   consequent are dependent on one another. The interesting points are following:
#   1. Antecedent: afternoon, coffee, Extra Salami or Feta
#      Consequent: Salad
#      I think this case is reasonable.
#   2. Buying Postcard, t-shirt in evening at weekend. I'm not sure the reason, 
#      but it's maybe tourist buying postcard and t-shirt for souvenir.
#   3. Afternoon with the baker (afternoon tea) in evening at weekend. 
#      I think it's reasonable too.
# However, it's great to remind that those cases aren't frequent.

## Rule2 ------------------------------------------------
inspect(head(sort(rule2, by ="lift"), 50))


# Bottom Support ------------------------------------------------------
inspect(head(sort(rule3, by ="support", decreasing = FALSE), 50))
# The interesting points are following:
#   1. Buying postcard is really rare case. (but have high lift when consider lift)
#   2. It show combination of products that least frequent, such as 
#      2.1 Bread and Extra Salami or Feta (negative effect from lift too)
#      2.2 Salad and Juice
#      2.3 Jammie Dodgers (British biscuit) and Cookies
#      2.3 Pastry in evening (negative effect from lift too)
# I think it's reasonable overall. Some products are redundant or incompatible, 
#   so people will not buy them together.


# Bottom Confidence ---------------------------------------------------------
inspect(head(sort(rule3, by ="confidence", decreasing = FALSE), 50))
# It show rare cases, so it's tricky to interpret. Moreover, some rare cases are 
#   the products that explained in previous parts.


# Bottom Lift ------------------------------------------------------------------
inspect(head(sort(rule3, by ="lift", decreasing = FALSE), 50))
# Inspect bottom lift means that these cases are negative effect between 
#   antecedent and consequence. The interesting points are following:
# 1. Buying soup in morning at weekday. It's quite reasonable, and people rather 
#    buy soup in afternoon at weekday according to 'Inspection by confidence part'.
# 2. Buying duplicate or incompatible food. (Both products are quite the same) For Example:
#    2.1 Sandwich and Pastry
#    2.2 Sandwich and Bread
#    3.3 Coffee and Coke
# 3. Farm House in many cases, but I don't understand what it is.
# 4. Some combination of products in morning at weekday that I'm sure the reasons. For Example:
#    4.1 Scone with Coffee in morning at weekday.
#    4.2 Sandwich with Coffee in morning at weekday.
#    4.3 Sandwich in morning.


# Promotion --------------------------------------------------------------------
# Idea: Find product that have high confidence/lift with coffee, but they aren't
#       top frequently enough.
# Promotion: Buying Coffee with Toast with some discount
# Reason:
#   1. From the item frequency graph, toast is 21th product, and less than bread, 
#      cake, pastry, and sandwich which are frequently paired with coffee.
#   2. According to result of top confidence, confidence of buying toast and coffee
#      is quite high, more than 0.7. (37th-39th)  
#   3. According to result of top lift using rule2, lift of buying toast and coffee
#      is decent, more than 1.5. (31th-35th) 




