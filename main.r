data <- read.csv("https://raw.githubusercontent.com/dat-analytics/data_access_2_t2_2023/main/z5373431_z5373431-Assessment2Data.csv", encoding="UTF-8")
data
trimmedData <- data[,2:14]
trimmedData

#########################################################################################################################
## Made data visualizations on each column, the C_Gender one is kind of the most complicated but can act as a template ##
## if we want to make it look better for the final output                                                              ##
#########################################################################################################################

# Total Spend
boxplot(trimmedData$C_TotalSpend)

# Gender
male <- nrow(trimmedData[trimmedData$C_Gender == 'M',])
female <- nrow(trimmedData[trimmedData$C_Gender == 'F',])
pie(c(male,female),c('53.65%','46.35%'),col=c("#8b984e","#5a5c5a"),cex=1.3)
legend("topright", c("Male","Female"), fill = c("#8b984e","#5a5c5a"))
title(main="Gender", cex.main=1.5)

# Age
boxplot(trimmedData$C_Age)

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
# Posted a question regarding this variable, I think its just a typo in the dictionary

# App Referral
barplot(table(trimmedData$App_Referral))
referralYes <- nrow(trimmedData[trimmedData$App_Referral > 0,])
referralNo <- 2000 - referralYes
pie(c(referralYes,referralNo),c('21.05%','78.95%'),col=c("#8b984e","#5a5c5a"),cex=1.3)
legend("topright", c("Has Referral","No Referral"), fill = c("#8b984e","#5a5c5a"))
title(main="Referral vs No Referral", cex.main=1.5)

# App Promotion
barplot(table(trimmedData$App_Promotion))
