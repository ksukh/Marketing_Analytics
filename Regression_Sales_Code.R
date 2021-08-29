library(xlsx)
ad.df <- read.xlsx("C:/Documents/adv_sales.xlsx", sheetIndex = 1, header=TRUE)
ad.df <- ad.df[-c(1)]

#Check for null values
sum(is.na(ad.df))

#General descriptive statistics
summary(ad.df)

#Generalized pair plots
library(gpairs)
gpairs(ad.df)

#Correlation
library(corrplot)
corrplot.mixed(cor(ad.df[ , c(1:7)]), upper="ellipse")

#add new column total spending on advertisement
#install.packages("magrittr") # package installations are only needed the first time you use it
#install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
sales.df <- sales.df %>% mutate(Total_Adv_Spend = store+billboard+printout)

#Total advertising spending vs sales
reg1 <- lm(Total_Adv_Spend~sales, data=sales.df) 
summary(reg1)
plot(Total_Adv_Spend~sales, data=sales.df, xlab="Overall Sales", ylab="Overall Advertising",col='blue')
abline(reg1)


reg2 <- lm(store~sales, data=sales.df) 
summary(reg2)
plot(store~sales, data=sales.df, xlab="Overall Sales", ylab="Store",col='blue')
abline(reg2)

#Billboard VS Sales
reg3 <- lm(billboard~sales, data=sales.df) 
summary(reg3)
plot(billboard~sales, data=sales.df, xlab="Overall Sales", ylab="Billboard Spending",col='blue')
abline(reg3)

#printout VS Sales
reg4 <- lm(printout~sales, data=sales.df) 
summary(reg4)
plot(printout~sales, data=sales.df, xlab="Overall Sales", ylab="printout Spending",col='blue')
abline(reg4)

# Bi-variate plots - Competitor Advertising spending

reg5 <- lm(comp~sales, data=sales.df) 
summary(reg5)
plot(comp~sales, data=sales.df, xlab="Overall Sales", ylab="Competitor Spending",col='blue')
abline(reg5)


#Satisfaction level  VS Sales
reg6 <- lm(sat~sales, data=sales.df) 
summary(reg6)
plot(sat~sales, data=sales.df, xlab="Overall Sales", ylab="Satisfaction level",col='blue')
abline(reg6)


#Satisfaction level  VS Sales
reg7 <- lm(price~sales, data=sales.df) 
summary(reg7)
plot(price~sales, data=sales.df, xlab="Overall Sales", ylab="Price",col='blue')
abline(reg7)

#Split train and test sets (75-25% split)
train.df<-ad.df[1:750,]
test.df<-ad.df[751:1000,]

#Model Development
m1.train <- lm(sales~price, data = train.df)
summary(m1.train)

m2.train <- lm(sales~price + store, data = train.df)
summary(m2.train)

m3.train <- lm(sales~price + store + billboard, data = train.df)
summary(m3.train)

m4.train <- lm(sales~price + store + billboard + printout, data = train.df)
summary(m4.train)

m5.train <- lm(sales~price + store + billboard + printout + sat, data = train.df)
summary(m5.train)

m6.train <- lm(sales~price + store + billboard + printout + sat + comp, data = train.df)
summary(m6.train)

m7.train <- lm(sales~price+sat+comp+printout+(store+billboard)^2, data=train.df)
summary(m7.train)

m8.train <- lm(sales~price+sat+comp+(store+billboard+printout)^3, data=train.df)
summary(m8.train)

library(ggplot2)
library(coefplot)
coefplot(m8.train, intercept=FALSE, outerCI=1.96, lwdOuter=1.5, ylab="Rating of Feature", xlab="Association with Overall Sales")

#Prediction
m1.test<-predict(m1.train,test.df)
m2.test<-predict(m2.train,test.df)
m3.test<-predict(m3.train,test.df)
m4.test<-predict(m4.train,test.df)
m5.test<-predict(m5.train,test.df)
m6.test<-predict(m6.train,test.df)
m7.test<-predict(m7.train,test.df)
m8.test<-predict(m8.train,test.df)

# Compute R-squared in the test sample 
# R-squared = Explained variation / Total variation

SSE = sum((test.df$sales - m1.test)^2) # Explained variation
SST = sum((test.df$sales - mean(test.df$sales))^2) # Total Variation
Rsq1=1 - SSE/SST

cor(test.df$sales, m2.test)^2

SSE2 = sum((test.df$sales - m2.test)^2)
Rsq2=1 - SSE2/SST

SSE3 = sum((test.df$sales - m3.test)^2)
Rsq3=1 - SSE3/SST

SSE4 = sum((test.df$sales - m4.test)^2)
Rsq4=1 - SSE4/SST

SSE5 = sum((test.df$sales - m5.test)^2)
Rsq5=1 - SSE5/SST

SSE6 = sum((test.df$sales - m6.test)^2)
Rsq6=1 - SSE6/SST

SSE7 = sum((test.df$sales - m7.test)^2)
Rsq7=1 - SSE7/SST

SSE8 = sum((test.df$sales - m8.test)^2)
Rsq8=1 - SSE8/SST

# visualize the fitted values 

plot(test.df$sales,type='l', col="blue", xlab=NA, ylab=NA)
# Create a title with a red, bold/italic font
title(main="Holdout Prediction", col.main="red", font.main=4)
# Graph trucks with red dashed line and square points
lines(m7.test, col="red")
# Label the x and y axes with dark green text
title(xlab="Time", col.lab=rgb(0,0.5,0))
title(ylab="Sales", col.lab=rgb(0,0.5,0))
