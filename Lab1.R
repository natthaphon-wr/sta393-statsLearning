# Set working directory in Rstudio, >Session > Set working directory
#getwd()
#setwd("C:/Users/NATT/Documents/KMUTT_Ongoing-up-to-date/Teaching/65-2/STA393/R programs")

#------------------------- Important Note -------------------------------------
# Order of run command is significant! (not 100% unique parameter name)

#------------------------- Import Data ---------------------------------------
z <- read.table("galton.txt")

head(z)
help(read.table)

# no header in the file,variables are named V1 and V2
z <- read.table("galton.txt",header=T,na.strings ="?")
head(z)

#------------------------- Explore Data ------------------------------------
names(z)
z$Father
z$Height

z$Height[5]
Height

attach(z)
Father
Mother
Father[1]


dim(z)
names(z)
?hist
hist(Father)
hist(z$Height)
pairs(z[,-c(1,4)])

summary(z)

?cor
cor(z[,c(2,3,5)])
cov(z[,c(2,3,5)])
cor # type e.g. cor, produces source code
boxplot(Father)
boxplot(Height)
lmts <- range(Father,Height)
lmts

par(mfrow = c(1, 2))
boxplot(Father,ylim=lmts)
boxplot(Height,ylim=lmts)
plot(Father,Height) # scatter plot

Gender=="M"
sum(Gender=="M")
z[Gender=="M",]
par(mfrow = c(1, 1))
plot(Father[Gender=="M"],Height[Gender=="M"],ylab="Son's Height",xlab="Father's Height", main="Galton's Regression")
plot(Father[Gender=="F"],Height[Gender=="F"],ylab="Daughter's Height",xlab="Father's Height", main="Galton's Regression")


#----------------------------- Model 1 -----------------------------
# Father--->Child

model1 <- lm(Height ~ Father , data = z)
model1$coefficients
summary(model1)

#plot regression
par(mfrow = c(1, 1))
plot(Father,Height, 
     main="Linear Regression from Model 1", 
     xlab="Father's Height", 
     ylab="Child's Height")
abline(model1$coefficients[1],model1$coefficients[2],col='red')

#plot regression residuals
plot(Father, model1$residuals, ylab="Residuals", xlab="Father's Height", main="Galton's Residuals")
abline(0, 0,col='red') # horizon line

#check if residuals follow normal
#histogram
hist(scale(model1$residuals))
#Q-Q
qqnorm(scale(model1$residuals), main='Normal')
qqline(scale(model1$residuals))
#Shapiro-Wilk Test
shapiro.test(scale(model1$residuals))
#Kolmogorov-Smirnov
ks.test(scale(model1$residuals), 'pnorm')


#---------------------------- Model 2 --------------------------------------
# Father--->Daughter

model2<-lm(Height[Gender == 'F'] ~ Father[Gender == 'F'])
model2$coefficients
summary(model2)

#plot regression
par(mfrow = c(1, 1))
plot(Father[Gender == 'F'],Height[Gender == 'F'], 
     main="Linear Regression from Model 2", 
     xlab="Father's Height", 
     ylab="Daughter's Height")
abline(model2$coefficients[1],model2$coefficients[2],col='red')

#plot regression residuals
plot(Father[Gender == 'F'], model2$residuals, ylab="Residuals", xlab="Father's Height", main="Galton's Residuals")
abline(0, 0,col='red')                  # the horizon

#check if residuals follow normal
#histogram
hist(scale(model2$residuals))
#Q-Q
qqnorm(scale(model2$residuals), main='Normal')
qqline(scale(model2$residuals))
#Shapiro-Wilk Test
shapiro.test(scale(model2$residuals))
#Kolmogorov-Smirnov
ks.test(scale(model2$residualss), 'pnorm')

#------------------------------ Model 3 ------------------------------------
# Mother--->Daughter

model3<-lm(Height[Gender == 'F'] ~ Mother[Gender == 'F'])
model3$coefficients
summary(model3)

#plot regression
par(mfrow = c(1, 1))
plot(Mother[Gender == 'F'],Height[Gender == 'F'], 
     main="Linear Regression from Model 3", 
     xlab="Mother's Height", 
     ylab="Daughter's Height")
abline(model3$coefficients[1], model3$coefficients[2], col='red')

#plot residual
plot(Mother[Gender == 'F'], model3$residuals, ylab="Residuals", xlab="Mother's Height", main="Galton's Residuals")
abline(0, 0,col='red')   

#check if residuals follow normal
#histogram
hist(scale(model3$residuals))
#Q-Q
qqnorm(scale(model3$residuals), main='Normal')
qqline(scale(model3$residuals))
#Shapiro-Wilk Test
shapiro.test(scale(model3$residualss))
#Kolmogorov-Smirnov
ks.test(scale(model3$residuals), 'pnorm')


#-------------------------- Model 4 ----------------------------------------
# Father+Gender--->Child

model4<-lm(Height ~ Father + Gender)
model4$coefficients
summary(model4)

#plot regression Daughter
par(mfrow = c(1, 1))
plot(Father[Gender == 'F'],Height[Gender == 'F'], 
     main="Linear Regression from Model 4", 
     xlab="Father's Height",
     ylab="Daughter's Height")
abline(model4$coefficients[1],model4$coefficients[2],col='red')


#Plot regression son
par(mfrow = c(1, 1))
plot(Father[Gender == 'M'],Height[Gender == 'M'],
     main="Linear Regression from Model 4", 
     xlab="Father's Height",
     ylab="Son's Height")
abline(model4$coefficients[1]+model4$coefficients[3],model4$coefficients[2],col='red')


#check if residuals follow normal
#histogram
hist(scale(model4$residuals))
#Q-Q
qqnorm(scale(model4$residuals), main='Normal')
qqline(scale(model4$residuals))
#Shapiro-Wilk Test
shapiro.test(scale(model4$residuals))
#Kolmogorov-Smirnov
ks.test(scale(model4$residuals), 'pnorm')


#------------------------------ Model 5 --------------------------------
# Father+Mother+Gender--->Child  

cor(Father,Mother) #see correlation between Father's Height and Mother's Height

model5<-lm(Height ~ Father+Mother+Gender)
summary(model5)

#check if residuals follow normal
#histogram
hist(scale(model5$residuals))
#Q-Q
qqnorm(scale(model5$residuals), main='Normal')
qqline(scale(model5$residuals))
#Shapiro-Wilk Test
shapiro.test(scale(model5$residuals))
#Kolmogorov-Smirnov
ks.test(scale(model5$residuals), 'pnorm')


#-------------------------- Model 6 -----------------------------
# Interaction mode: Father*Mother+Gender--->Child  

cor(Father,Mother) #see correlation between Father's Height and Mother's Height

model6<-lm(Height ~ Father*Mother+Gender)
summary(model6)

#check if residuals follow normal
#histogram
hist(scale(model6$residuals))
#Q-Q
qqnorm(scale(model6$residuals), main='Normal')
qqline(scale(model6$residuals))
#Shapiro-Wilk Test
shapiro.test(scale(model6$residuals))
#Kolmogorov-Smirnov
ks.test(scale(model6$residuals), 'pnorm')


#---------------------- Model 7 -----------------------------------
# Avg(Father, Mother)+Gender--->Child

#create new column
z$MeanHeight <- rowMeans(z[,c('Father', 'Mother')], na.rm=TRUE)
head(z)
attach(z)
MeanHeight

#create model
model7 <- lm(Height ~ MeanHeight+Gender)
summary(model7)

#plot regression Daughter
par(mfrow = c(1, 1))
plot(MeanHeight[Gender == 'F'], Height[Gender == 'F'], 
     main="Linear Regression from Model 7", 
     xlab="Average Height of Father and Mother",
     ylab="Daughter's Height")
abline(model7$coefficients[1],model7$coefficients[2],col='red')


#Plot regression son
par(mfrow = c(1, 1))
plot(MeanHeight[Gender == 'M'], Height[Gender == 'M'],
     main="Linear Regression from Model 4", 
     xlab="Average Height of Father and Mother",
     ylab="Son's Height")
abline(model7$coefficients[1]+model7$coefficients[3],model7$coefficients[2],col='red')

#check if residuals follow normal
#histogram
hist(scale(model7$residuals))
#Q-Q
qqnorm(scale(model7$residuals), main='Normal')
qqline(scale(model7$residuals))
#Shapiro-Wilk Test
shapiro.test(scale(mode76$residuals))
#Kolmogorov-Smirnov
ks.test(scale(model7$residuals), 'pnorm')


#-------------------------- Model 8 -----------------------------------
#------------ Model 8.1 ------------
#Polynomial of Father+Gender--->Child

#loop create model and keep track adjusted r squared
adjRSq_list1 = list()
for (i in 1:10){
  model8_1 = lm(Height ~ poly(Father, i) + Gender)
  adjrsq = summary(model8_1)$adj.r.squared
  adjRSq_list1<-append(adjRSq_list1, adjrsq)
}
  
#plot adjusted r squared vs polynomial degree
plot(1:10, adjRSq_list1,
     main="Polynomial with Only Father",
     xlab="Polynomial Degree",
     ylab="Adjusted R Squared")
sprintf("The Maximum Adjusted R Squared is %f at %d polynomal degree", 
      max(unlist(adjRSq_list1)), which.max(adjRSq_list1))

#------------ Model 8.2 ------------
# Polynomial of Mother+Gender--->Child

#loop create model and keep track adjusted r squared
adjRSq_list2 = list()
for (i in 1:10){
  model8_2 = lm(Height ~ poly(Mother, i) + Gender)
  adjrsq = summary(model8_2)$adj.r.squared
  adjRSq_list2<-append(adjRSq_list2, adjrsq)
}

#plot adjusted r squared vs polynomial degree
plot(1:10, adjRSq_list2,
     main="Polynomial with Only Mother",
     xlab="Polynomial Degree",
     ylab="Adjusted R Squared")
sprintf("The Maximum Adjusted R Squared is %f at %d polynomal degree", 
        max(unlist(adjRSq_list2)), which.max(adjRSq_list2))

#------------ Model 8.3 ------------
# Polynomial of Father+Mother+Gender--->Child
# Note: 2 for loop for father and mother degree, so there are total 10*10=100 models

adjRSq_M8_3 = matrix(nrow=10, ncol=10) #create empty matrix for keep track adj r squared

#loop for each degree (Father is i, Mother is j)
for (i in 1:10){
  for(j in 1:10){
    model8_3 = lm(Height ~ poly(Father, i) + poly(Mother, j) + Gender)
    adjrsq = summary(model8_3)$adj.r.squared
    adjRSq_M8_3[i, j] = adjrsq
  }
}

#maximum adjusted R Squared
max_degree_8_3 = which(adjRSq_M8_3 == max(adjRSq_M8_3), arr.ind = TRUE)
sprintf("The Maximum Adjusted R Squared is %f at %d polynomial degree of Father and %d polynomial degree of Mother", 
        max(adjRSq_M8_3), max_degree_8_3[1], max_degree_8_3[2])

#plot adjusted r squeared for each value in matrix
library('plot.matrix')
plot(adjRSq_M8_3, 
     axis.col=1, 
     axis.row=0.5,
     digits=4, 
     text.cell=list(cex=0.5),
     breaks=10,
     key=NULL,
     main="Adjusted R Squared",
     xlab="Polynomial Degree of Mother",
     ylab="Polynomial Degree of Father",
    )


#-------------------------- Model 9 -----------------------------------
#Represent Only one child (for daughter and son) in each family

#manipulate data frame
library(dplyr)
summarize_df <- z %>%
  group_by(Family, Gender) %>%
  summarize(Father = first(Father),
            Mother = first(Mother),
            mean_height = mean(Height))
summarize_df
attach(summarize_df)

#create model
model9<-lm(mean_height ~ Father+Mother+Gender)
summary(model9)


#-------------------------- Model 10 -----------------------------------
# Polynomial of data that Only one child (for daughter and son) in each family
# Polynomial of Father+Mother+Gender--->Child

adjRSq_M10 = matrix(nrow=10, ncol=10) #create empty matrix for keep track adj r squared

#loop for each degree (Father is i, Mother is j)
for (i in 1:10){
  for(j in 1:10){
    model10 = lm(mean_height ~ poly(Father, i) + poly(Mother, j) + Gender)
    adjrsq = summary(model10)$adj.r.squared
    adjRSq_M10[i, j] = adjrsq
  }
}

#maximum adjusted R Squared
max_degree_10 = which(adjRSq_M10 == max(adjRSq_M10), arr.ind = TRUE)
sprintf("The Maximum Adjusted R Squared is %f at %d polynomial degree of Father and %d polynomial degree of Mother", 
        max(adjRSq_M10), max_degree_10[1], max_degree_10[2])

#plot adjusted r squeared for each value in matrix
library('plot.matrix')
plot(adjRSq_M10, 
     axis.col=1, 
     axis.row=0.5,
     digits=4, 
     text.cell=list(cex=0.5),
     breaks=10,
     key=NULL,
     main="Adjusted R Squared (One Son/Daughter per Family)",
     xlab="Polynomial Degree of Mother",
     ylab="Polynomial Degree of Father",
)

