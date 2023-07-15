library(readr)
library(dplyr)
library(moments)
library(rpart)
library(rpart.plot)

data <- read.csv("https://raw.githubusercontent.com/dat-analytics/data_access_2_t2_2023/main/z5373431_z5373431-Assessment2Data.csv", encoding="UTF-8")
data
trimmedData <- data[,2:14]
trimmedData

#########################################################################################################################
## Made data visualizations on each column, the C_Gender one is kind of the most complicated but can act as a template ##
## if we want to make it look better for the final output                                                              ##
#########################################################################################################################

#########################################################################################################################
## Descriptive Analysis                                                                                                ##
#########################################################################################################################

# Total Spend
boxplot(trimmedData$C_TotalSpend)
mean(trimmedData$C_TotalSpend)
median(trimmedData$C_TotalSpend)

# Gender
male <- nrow(trimmedData[trimmedData$C_Gender == 'M',])
female <- nrow(trimmedData[trimmedData$C_Gender == 'F',])
pie(c(male,female),c('53.65%','46.35%'),col=c("#8b984e","#5a5c5a"),cex=1.3)
legend("topright", c("Male","Female"), fill = c("#8b984e","#5a5c5a"))
title(main="Gender", cex.main=1.5)

# Age
boxplot(trimmedData$C_Age)
min(trimmedData$C_Age)
max(trimmedData$C_Age)
mean(trimmedData$C_Age)
median(trimmedData$C_Age)
kurtosis(trimmedData$C_Age)

age_10s <- nrow(trimmedData[trimmedData$C_Age > 9 & trimmedData$C_Age < 20,])
age_20s <- nrow(trimmedData[trimmedData$C_Age > 19 & trimmedData$C_Age < 30,])
age_30s <- nrow(trimmedData[trimmedData$C_Age > 29 & trimmedData$C_Age < 40,])
age_40s <- nrow(trimmedData[trimmedData$C_Age > 39 & trimmedData$C_Age < 50,])
age_50s <- nrow(trimmedData[trimmedData$C_Age > 49 & trimmedData$C_Age < 60,])
age_60s <- nrow(trimmedData[trimmedData$C_Age > 59 & trimmedData$C_Age < 70,])

barplot(c(age_10s,age_20s,age_30s,age_40s,age_50s,age_60s), xlab = "Age", ylim = c(0,1200), names.arg = c("10s","20s","30s","40s","50s","60s"))

# State
states <- table(trimmedData$C_State)
barplot(states)

# Area
barplot(table(trimmedData$C_Area))

# Product 
barplot(table(trimmedData$C_Product))

# Device Type
barplot(table(trimmedData$C_DeviceType))

# Shopping Duration
barplot(table(trimmedData$C_ShoppingDuration), xlab = "Minutes")

# Time of Shopping
barplot(table(trimmedData$C_TimeOfShopping))

# App Satisfaction
barplot(table(trimmedData$App_SatisfactionRating))

# App Tenure
barplot(table(trimmedData$App_Tenure), xlab = "Days")
# Posted a question on the forum regarding this variable, I think its just a typo in the dictionary
mean(trimmedData$App_Tenure)
median(trimmedData$App_Tenure)
kurtosis(trimmedData$App_Tenure)

# App Referral
barplot(table(trimmedData$App_Referral))
referralYes <- nrow(trimmedData[trimmedData$App_Referral > 0,])
referralNo <- 2000 - referralYes
pie(c(referralYes,referralNo),c('21.05%','78.95%'),col=c("#8b984e","#5a5c5a"),cex=1.3)
legend("topright", c("Has Referral","No Referral"), fill = c("#8b984e","#5a5c5a"))
title(main="Referral vs No Referral", cex.main=1.5)

# App Promotion
barplot(table(trimmedData$App_Promotion))

# Box plot of Customer Satisfaction and Spending
satisfaction <- replace(trimmedData$App_SatisfactionRating, trimmedData$App_SatisfactionRating == "H", 1)
satisfaction <- replace(satisfaction, satisfaction == "L", 0)
# 1 here represents 'H' and 0 represents 'L'
spending <- trimmedData$C_TotalSpend
boxplot(spending ~ satisfaction)
satisfaction <- as.numeric(satisfaction)
cor(satisfaction, spending)

# Box plot of Customer Satisfaction and Referrals
referral <- trimmedData$App_Referral
boxplot(referral ~ satisfaction)
referralSatisfaction <- trimmedData[, c('App_SatisfactionRating','App_Referral')]
referralSatisfied <- referralSatisfaction[referralSatisfaction$App_SatisfactionRating == "H",]
referralNotSatisfied <- referralSatisfaction[referralSatisfaction$App_SatisfactionRating == "L",]

mean(referralSatisfied$App_Referral)
mean(referralNotSatisfied$App_Referral)
# Data suggests that with high satisfaction ratings, customers are more likely to refer ASAL's products and app

# Box plot of Referrals and Promotion
yesPromotion <- trimmedData[trimmedData$App_Promotion == "Yes",]
noPromotion <- trimmedData[trimmedData$App_Promotion == "No",]

summary(yesPromotion)
table(yesPromotion$App_SatisfactionRating)
summary(noPromotion)
table(noPromotion$App_SatisfactionRating)

promotion <- trimmedData$App_Promotion
promotion <- replace(promotion, promotion == "Yes", 1)
promotion <- replace(promotion, promotion == "No", 0)
promotion <- as.numeric(promotion)
cor(promotion, spending)

#########################################################################################################################
## Predictive Analysis                                                                                                 ##
#########################################################################################################################

trimmedData
# Make a dummy variable for the satisfied condition of the variable
trimmedData$Satisfied <- ifelse(trimmedData$App_SatisfactionRating == "H", 1,0)
trimmedData
set.seed(1)
# 70/30 train test split
split <- 0.70*2000
train <- sample(2000, split)
trimmedDataTrain <- trimmedData[train,]
trimmedDataTest <- trimmedData[-train,]

# Decision Tree
tree <- rpart(App_SatisfactionRating ~ App_Referral + App_Tenure, data = trimmedDataTrain)
rpart.plot(tree, yesno = 2)
tree <- rpart(App_SatisfactionRating ~ C_Age + App_Referral, data = trimmedDataTrain)
rpart.plot(tree, yesno = 2)

# Logistic Regression for Satisfaction
logistic <- glm(Satisfied ~ App_Referral + C_TotalSpend, family = binomial(), data = trimmedDataTrain)
summary(logistic)