# Assignment 2

# Problems: 2.20, 2.21, 2.22, 2.30 (using R) and 2.31, 2.32 (by hand)

library(readr)
library(readxl)

#2.30: You may need to do an online search in order to find suitable R code for this problem. Please site your resource in the R file
#2.31: Here "prove" means "explain why" in a rigorous manner using equations to support your claims.
#2.32: This problem is a generalization of the discussion in section 2.10. There it is assumed that beta_0 = 0. For this problem it is assumed that beta_0 is a known (but possibly nonzero) value.

setwd('C:\\Users\\Prasanth\\Downloads\\M.S. Data Science Academics\\1st Sem\\Linear Regression\\Assignments\\Assignment 2')

fuel_data <- read_excel("data-table-B18.xls")

# problem 2.20

regression1 <- lm(y ~ x5, data = fuel_data)

summary(regression1)

# the equation is y = 410.72 - 0.263*inital boiling point

# R square is 0.34

# x5 is significant. P value is 0.01

# F value is 7.51 which is quite low

# intial boiling point is important predictor but there is still lot of unexplained variance

# problem 2.21

wine_data <- read_excel("data-table-B19.xls")

regression2 <- lm(y ~ x3, data = wine_data)
  
summary(regression2)

# the R square is 0.14

# P value of sulphur content is significant

# The F statistic is quite low: 4.936

# There is a relation between rating and sulphur content. But there is a lot of unexplained variance

# problem 2.22

methanol_data <- read_excel("data-table-B20.xls")

regression3 <- lm(y ~ x5, data = methanol_data)

summary(regression3)

# The R square is 0.0133. There is not really a relationship

# The F statistic is 0.216

# problem 2.30

Month <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

Temperature <- c(21, 24, 32, 47, 50, 59, 68, 74, 62, 50, 41, 30)

Usage <- c(185.79, 214.47, 288.03, 424.84, 454.68, 539.03, 621.55, 675.06, 562.03, 452.93, 369.95, 273.98)

dat <- data.frame(Month, Temperature, Usage)

dat$Month <- as.factor(dat$Month)

correlation = cor(Usage, Temperature)

# Nearly 1

# part b 

t = correlation*sqrt(12 - 2)/sqrt(1 - (correlation)^2)

# 272.22

# part c

# at corr = 0.5

# I saw these formulas on page 57 in the book

arctanh = function(x) {
  arctanh = 0.5*log((1 + x)/(1-x))
  return(arctanh)
}

Z = (arctanh(0.99) - arctanh(0.5))*sqrt(12 - 3)

Z

# 6.29

# part d

tanh = function(x) {
  tanh = ((exp(x) - exp(-x))/(exp(x) + exp(-x)))
  return(tanh)
}
  
# upper limit 

tanh(arctanh(0.99) + 2.57/sqrt(9))

# 0.9981

tanh(arctanh(0.99) - 2.57/sqrt(9))

# 0.945
