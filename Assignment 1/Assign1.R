setwd("C:\\Users\\Prasanth\\Downloads\\M.S. Data Science Academics\\1st Sem\\Linear Regression\\Assignments\\Assignment 1")

library(readr)
library(dplyr)

B1 <- read_excel("B1.xls")
B3 <- read_excel("B3.xls")

# problem 2.1

# part a

model1 <- lm(y ~ x8, data = B1)

summary(model1)

# The final model: y = 21.78 -0.007025x8

# part b

anova(lm(y ~ x8, data = B1))

# Analysis of Variance Table

# Response: y
#           Df  Sum Sq   Mean Sq   F value    Pr(>F)    
#x8         1   178.09   178.092   31.103    7.381e-06 ***
#Residuals  26  148.87    5.726                      

# The R square is quite high and p value is very low. I would say the regression is significant

# part c ?

confint(lm(y ~ x8, data = B1))

#  -0.009614347 -0.004435854

# part D

#54.47%

# part E

X6 <- data.frame(x8 = 2000)

predict(model1, X6, interval="confidence", level=0.95)

# Lower: 6.76  Upper: 8.7103

# Problem 2.2

# the point estimate will be 

# 21.78 -0.007025*1800 

# 9.135

X7 <- data.frame(x8 = 1800)

predict(model1, X7, interval="prediction", level=0.90)

# lwr 4.93 Upper limit 13.34


# problem 2.4

#problem a

model3 <- lm(y ~ x1, data = B3)

summary(model3)

# y = 33.7 - 0.0473x1

#problem b

anova(lm(y ~ x1, data = B3))

#           Df Sum Sq  Mean Sq  F value    Pr(>F)    
#x1          1 955.72   955.72  101.74    3.743e-11 ***
#Residuals  30 281.82    9.39                      

#problem c

# 77.23 percentage

# problem d

X8 <- data.frame(x1 = 275)

predict(model3, X8, interval="confidence", level=0.95)

# Lower limit 19.58 Upper Limit 21.809

# problem e

X9 <- data.frame(x1 = 275)

predict(model3, X9, interval="prediction", level=0.95)

# lower limit 14.34 Upper Limit 27.05

#problem f

# Part D is confidence interval. Part E is the prediction interval. As expected, prediction level is larger than confidence level. In part d, we are only estimating how confident we are about the mean value. In part e, we are predicting the range based on a completely new value the model didn't adjust for originally.

# Problem 2.5

model4 <- lm(y ~ x10, data = B3)

summary(model4)

# y = 40.85 -0.0057516x1

#problem b

anova(lm(y ~ x10, data = B3))

#          Df  Sum Sq  Mean Sq  F value    Pr(>F)    
#x10        1  921.53  921.53   87.482    2.121e-10 ***
#Residuals 30  316.02  10.53                      

#problem c

# 74.46 percentage

# I don't think we can draw any conclusions just by looking at R square. Moreover even the R squares are pretty close. Both models seem to be highly significant. We need to dig deep into the data set and look at which variables are highly correlated and how this is affecting the results of our model.

# problem 2.12

# part a

Month <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

Temperature <- c(21, 24, 32, 47, 50, 59, 68, 74, 62, 50, 41, 30)

Usage <- c(185.79, 214.47, 288.03, 424.84, 454.68, 539.03, 621.55, 675.06, 562.03, 452.93, 369.95, 273.98)

dat <- data.frame(Month, Temperature, Usage)

dat$Month <- as.factor(dat$Month)

model7 <- lm(Usage ~ Temperature, data = dat)

model7

# Usage = -6.33 + 9.2Temperature

#part b

summary(model7)

#part c

# is this hypothesis testing?

# H0: slope = 10,000 lb

# H1: slope != 10,000 lb

# since I am using usage directly, I will use just 10 for my testing

coef(summary(model7))[, "Std. Error"] 

# Now i know std error of slope is 0.03382

# t test 

(9.208 - 10) / 0.03382

-23.41

# The t value is really large. 

#The slope is definitely less than 10 (10,000 lb)

#part d

X10 <- data.frame( Temperature = 58)

predict(model7, X10, interval="prediction", level=0.99)

# Lower 521.22  Upper 534.2944