library(readxl)

# Problem 3.1

data1 <- read_excel("data-table-B1.xls")

# part a

model1 <- lm(y ~ x2 + x7 + x8, data = data1)

summary(model1)

# y = -1.8 + 0.0035x2 + 0.193x7 - 0.0048x8

# part b

summary(model1)

anova(model1)

# Regression is significant based on the F and p values

# Source       df     SS        MS      F    p-value
# Regression    3   257.094   85.698  29.44   0.000
# Error         24  69.87     2.911
# Total         27  326.964


# part c

summary(model1)

#       t-value     Pr(>|t|)
# x2     5.17       2.66e-05
# x7     2.19       0.037815
# x8     -3.7       0.000938

# all 3 variables are significant


# part d

# R square is 0.786, Adjusted R square is 0.7596


# part e

# F = (257 - 243)/2.91 = 4.84 which is a square of 2.19


# problem 3.3

# part a

confint(lm(y ~ x2 + x7 + x8, data = data1))

# 0.012 to 0.37

# part b

df <- data.frame(x2 = 2300, x7 = 56, x8 = 2100)

predict(model1, df, interval="confidence", level=0.95)

# 6.43 and 7.99


# problem 3.4

# part A

model2 <- lm(y ~ x7 + x8, data = data1)

summary(model2)

# y = 17.9 + 0.048 (x7) - 0.00654 (x8)

# With an F statistic of 15.13 and p value very small, the regresssion is significant

# part B

# R square is 0.54, Adj. R square is 0.51. These figures are much lower. X2 is adding a lot of value to model

# part c

confint(lm(y ~ x7 + x8, data = data1))

# -0.198 to 0.293

df2 <- data.frame(x7 = 56, x8 = 2100)

predict(model2, df2, interval="confidence", level=0.95)

# 5.82 to 8.02

# part d

# Removing an important variable affects standard errors of cofficeints and indirectly the value of R square


# problem 3.5

# part a

data2 <- read_excel("data-table-B3.xls")

model3 <- lm(y ~ x1 + x6, data = data2)

# y = 32.9 - 0.053 (x1) + (0.959) x6

# part b

summary(model3)

anova(model3)

# Source      df   SS       MS        F      p-value
# Regression  2   972.9     486.45  53.67     0.000
# Error       29  264.65    9.13
# Total       31  1237.54

# part c

summary(model3)

# R square is 0.78, Adjusted R square of 0.772.

# Even a simple linear regression gave a value of 0.77

# part D

confint(lm(y ~ x1 + x6, data = data2))

# -0.065 to -0.040

# part E

summary(model3)

# coff  t-test  p-value
#  x1    -8.66   0
#  x6    1.43    0.16

# part f

df3 <- data.frame(x1 = 275, x6 = 2)

predict(model3, df3, interval="confidence", level = 0.95)

# 18.87 to 21.5

# part g

predict(model3, df3, interval="predict", level = 0.95)

# 13.88 to 26.48


# problem 3.6

model2.4 <- lm(y ~ x1, data = data2)

predict(model2.4, df3, interval="confidence", level = 0.95)

# 19.58 to 21.80

predict(model2.4, df3, interval="predict", level = 0.95)

# 14.34 to 27

# Clearly x6 is not adding much value to the model


# problem 3.8

# part A

data4 <- read_excel("data-table-B5.xls")

model4 <- lm(y ~ x6 + x7, data = data4)

# y = 2.53 + 0.0185 (x6) + (2.19) (x7)

# part B

summary(model4)

# the F and p values indicate that the regression is significant

# R square is 0.699 and adjusted R square is 0.674

# part C

# coff   t stat   p-value
# x6      6.74     0.0
# x7      2.25     0.034

# part d

confint(model4)

# x6:  0.012 to 0.02
# x7: 0.178 to 4.19

# part e

model5 <- lm(y ~ x6, data = data4)

summary(model5)

# the test is significant and even the R square values are pretty close to the model which has x7 included too. I am satisfied with this model

# part f

confint(model5)

# 0.0133 to 0.0025

# these values are pretty close to the values obtained in part d. clearly x7 is not an important predictor

# part g

anova(model4) # Ms. res = 2573

anova(model5) # Ms. res = 5009

# the Ms. res is much lower with both variables added

# problem 3.16

# part A

data5 <- read_excel("data-table-B16.xls")

model6 <- lm(data5$LifeExp ~ data5$`People-per-TV` + data5$`People-per-Dr`)

summary(model6)

# life exp = 70.23 - 0.022(people per tv) - 0.0004470(people per doctor)

model7 <- lm(data5$LifeExpMale ~ data5$`People-per-TV` + data5$`People-per-Dr`)
  
summary(model7)  
  
# life exp for males = 73 - 0.0256 (people per tv) - 0.0004785 (people per doctor)

model8 <- lm(data5$LifeExpFemale ~ data5$`People-per-TV` + data5$`People-per-Dr`)

summary(model8)  

# life exp for females = 67.4 - 0.019 (people per tv) - 0.0004086 (people per doctor)

# In all the models, both the predictors are significant

# part B

summary(model6)

summary(model7)

summary(model8)

# Based on F and p values, all 3 models are significant

# part c

# based on the t tests on both variables in all the variables, they are significant all the time

# part d

# R square and adjusted for the 1st model: 0.434 and 0.402

# R square and adjusted for the 2nd model: 0.417 and 0.384

# R square and adjusted for the 3rd model: 0.445 and 0.414

# part e

confint(model6)

# For both males and females, it is -0.000856 to -3.777

confint(model7)

# For only the male model, -0.000947 to -1.008023e-05

confint(model8)

# For only the female model, -0.000767 to -5.007977e-05