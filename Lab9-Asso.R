# Import Library ------------------------
library(arules)
library(stringr)

# ptrans = read.csv("ptrans.csv",header=T,na.strings="?")
# fix(ptrans)
# ptrans[is.na(ptrans)]=0
# dim(ptrans)
# ptrans = ptrans[rowSums(ptrans)!=0,]
# 
# phist.asso = ptrans >0
# fix(phist.asso)
# 
# 0.05*5668
# grocery.rules3 = apriori(phist.asso, parameter = list(minlen=2,maxlen=5,support = 0.05, confidence = 0.5))
# ascus = inspect(head(sort(grocery.rules3, by = "confidence",decreasing = TRUE),20))

# Import Data --------------------------------------------------
grocery <- read.transactions('groceries.csv', sep=',')
dim(grocery)
fix(grocery)
inspect(grocery[size(grocery)==1])
inspect(grocery[size(grocery)==2])
inspect(grocery[size(grocery)<=10])
inspect(grocery[size(grocery)==32])
grocery[size(grocery)>10]

grocery.text <- readLines('groceries.csv')
sum(str_detect(grocery.text, "citrus fruit"))
sum(str_detect(grocery.text, "margarine"))
sum(str_detect(grocery.text, "citrus fruit") & str_detect(grocery.text, "margarine"))

# Manual Calculation --------------------------------------------------

#support 0.00793 or 0.793%
sum(str_detect(grocery.text, "citrus fruit") & str_detect(grocery.text, "margarine"))/9835
mean(str_detect(grocery.text, "citrus fruit") )

#confidence citrus fruit --> margarine 0.0958 or 9.58%
sum(str_detect(grocery.text, "citrus fruit") & str_detect(grocery.text, "margarine"))/sum(str_detect(grocery.text, "citrus fruit"))

#Lift 1.64
9835*sum(str_detect(grocery.text, "citrus fruit") & str_detect(grocery.text, "margarine"))/(sum(str_detect(grocery.text, "citrus fruit"))*sum(str_detect(grocery.text, "margarine")))

# Using Package ----------------------------------------------------------------
## Explore by Rule ----------------------------------
grocery.rules2 = apriori(grocery, parameter = list(minlen=2,maxlen=10,support = 0.001, confidence = 0.0))
ascus = inspect(head(sort(grocery.rules2, by = "lift",decreasing = TRUE),80))
ascus = inspect(head(sort(grocery.rules2, by = "confidence",decreasing = TRUE),80))

grocery.rules = apriori(grocery, parameter = list(minlen=2,maxlen=10,support = 0.005, confidence = 0.3))
length(grocery.rules)
ascus = inspect(head(sort(grocery.rules, by = "confidence",decreasing = TRUE),30))

## Inspect each variable --------------------------
### Milk and other vegetable ----------------------------
milk = 1:l
milk = milk[grocery.rules@rhs@itemInfo$labels[grocery.rules@rhs@data@i+1] =="whole milk" | grocery.rules@rhs@itemInfo$labels[grocery.rules@rhs@data@i+1] =="other vegetables"]
inspect(sort(grocery.rules[-milk],by = "confidence")) # NOT
inspect(head(sort(grocery.rules[milk], by = "confidence",decreasing = TRUE),100))
inspect(sort(grocery.rules[milk],by = "confidence"))

### Beer --------------------------
beer = 1:l
beer = beer[grocery.rules@rhs@itemInfo$labels[grocery.rules@rhs@data@i+1] =="canned beer" | grocery.rules@rhs@itemInfo$labels[grocery.rules@rhs@data@i+1] =="bottled beer"]
inspect(head(sort(grocery.rules[beer], by = "confidence",decreasing = TRUE),100))

## LAB SECTION -------------------------------------
sum(str_detect(grocery.text, "beef"))

### Define Rule ---------------------------
rule1 = apriori(grocery, parameter = list(minlen=1, maxlen=10, confidence=0.2, support=0.001))
rule2 = apriori(grocery, parameter = list(minlen=1, maxlen=10, confidence=0, support=0.001))
l1 = length(rule1)
l2 = length(rule2)
beef = 1:l1
beef = beef[rule1@rhs@itemInfo$labels[rule1@rhs@data@i+1] == "beef"]
beef2 = 1:l2
beef2 = beef2[rule2@rhs@itemInfo$labels[rule2@rhs@data@i+1] == "beef"]

### Inspect -------------------------------
# Positive association
inspect(head(sort(rule1[beef], by = "confidence",decreasing = TRUE), 20))
inspect(head(sort(rule1[beef], by = "lift",decreasing = TRUE), 20))
inspect(head(sort(rule1[beef], by = "support",decreasing = TRUE), 30))
# The products that people will buy with beef are usually food ingredients. 
# For example: other vegetables, rolls/buns, butter, cream cheese, yogurt.
# The interesting combination are "cat food, rolls/buns", it's seem like 
#   people will buy beef for cats.

# Negative association
inspect(head(sort(rule2[beef2], by = "confidence",decreasing = FALSE), 30))
inspect(head(sort(rule2[beef2], by = "lift",decreasing = FALSE), 30))
# The products that have negative association with beef are usually beverage or snack.
# For example: beer, candy, soda, coffee, chocolate, bottled water.
# This result are reasonable. 

### Promotion ---------------------------
# Pair with beef and rolls/buns or other ingredient with discount, 
#   when rolls/buns are almost expire and have a lots in stock. 


