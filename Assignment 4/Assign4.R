# Problems (all using R): 4.2a-c,e, 4.4a-b,d, 4.8, 4.13, 4.25, 4.29, 5.2, 5.5, 5.7

# part 4.2 a)

library(readxl)

data <- as.data.frame(read_excel("data-table-B1-1.xls"))

lm <- lm(y~., data=data) #Run the linear regression

qqnorm(rstudent(lm))
qqline(rstudent(lm))

# there are some points that are not on the curve. not really normal

# part 4.2 b)

Predicted_values = predict(lm, newdata = data)
standardized = rstandard(lm)
plot(Predicted_values, standardized)

# they seem to be pretty evenly distributed

# part 4.2 c)

plot(data$x1, standardized) # seems to be alright
plot(data$x2, standardized) # I think variance is slight larger for small x
plot(data$x3, standardized) # I think the variance gets bigger with x3
plot(data$x4, standardized) # seems to be alright
plot(data$x5, standardized) # seems to be alright
plot(data$x6, standardized) # seems to be alright
plot(data$x7, standardized) # I think variance gets bigger with x7
plot(data$x8, standardized) # seems to be alright
plot(data$x9, standardized) # seems to be alright

# part 4.2 e)

library("MASS") #need this package to calculate the studentized residual with studres()
studentized <- studres(lm); studentized #Find the studentized residuals
rstudent <-rstudent(lm); rstudent #Find the R-student residuals

plot(data$y, studentized) # there is a point beyond -2 (the first point)
plot(data$y, rstudent) # its the same. The first point is beyond -2



# problem 4.4 a)

data2 <- as.data.frame(read_excel("data-table-B3-1.xls"))

data2[is.na(data2)] <- 0 # i noticed there a couple of NA's

lm2 <- lm(y~., data=data2) #Run the linear regression

qqnorm(rstudent(lm2))
qqline(rstudent(lm2))

# not really normal

# part b)

Predicted_values = predict(lm2, newdata = data2)
standardized = rstandard(lm2)
plot(Predicted_values, standardized)

# we can argue there's a cluster below zero and polynomial relationship for large x

# part d)

library("MASS") #need this package to calculate the studentized residual with studres()
studentized <- studres(lm2); studentized #Find the studentized residuals
rstudent <-rstudent(lm2); rstudent #Find the R-student residuals

plot(data2$y, studentized)
plot(data2$y, rstudent) 

# there are few outliers in this data set

# problem 4.8

# part a

Month <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
Temperature <- c(21, 24, 32, 47, 50, 59, 68, 74, 62, 50, 41, 30)
Usage <- c(185.79, 214.47, 288.03, 424.84, 454.68, 539.03, 621.55, 675.06, 562.03, 452.93, 369.95, 273.98)
dat <- data.frame(Month, Temperature, Usage)
dat$Month <- as.factor(dat$Month)

lm3 <- lm(Usage ~ Temperature, data = dat)

qqnorm(rstudent(lm2))
qqline(rstudent(lm2))

# there are few outliers to the very right side. But the middle range seems to be normal

# part b

Predicted_values = predict(lm3, newdata = dat)
standardized = rstandard(lm3)
plot(Predicted_values, standardized)

# I don't think they are equally distributed around 0

# part c

plot(dat$Month, standardized)

# I think there is a pattern. It goes up and comes down again

# problem 4.13

data4 <- as.data.frame(read_excel("data-table-B5-1.xls"))

lm4 <- lm(y ~ data4$x6, data=data4) #Run the linear regression

summary(lm4) # R square is 0.63

lm5 <- lm(y ~ data4$x7, data=data4) #Run the linear regression

summary(lm5) # R square is 0.13

# performing residual analysis similar to earlier questions

qqnorm(rstudent(lm4))
qqline(rstudent(lm4))

# few outliers to the right, but everything seems to be alright

qqnorm(rstudent(lm5))
qqline(rstudent(lm5))

# hmm. I would say the earlier plot was better

Predicted_values = predict(lm4, newdata = data4)
standardized = rstandard(lm4)
plot(Predicted_values, standardized)

# i would say the variance goes up at the end

Predicted_values = predict(lm5, newdata = data4)
standardized = rstandard(lm5)
plot(Predicted_values, standardized)

# hmm. few outliers but looks great

# the press residuals

(r <- resid(lm4))
(pr <- resid(lm4)/(1 - lm.influence(lm4)$hat))
sum(r^2)
press<- sum(pr^2) #PRESS residual is 3692


(r <- resid(lm5))
(pr <- resid(lm5)/(1 - lm.influence(lm5)$hat))
sum(r^2)
press<- sum(pr^2) #PRESS residual is 8895

# I think we can reasonably say the model with x6 as predictor is better than the one with x7

# problem 4.25

data5 <- as.data.frame(read_excel("data-table-B16-1.xls"))

lm6 <- lm(data5$LifeExp ~ data5$`People-per-TV` + data5$`People-per-Dr`)
lm7 <- lm(data5$LifeExpMale ~ data5$`People-per-TV` + data5$`People-per-Dr`)
lm8 <- lm(data5$LifeExpFemale ~ data5$`People-per-TV` + data5$`People-per-Dr`)

qqnorm(rstudent(lm6))
qqline(rstudent(lm6))

# just one outlier. looks great otherwise

qqnorm(rstudent(lm7))
qqline(rstudent(lm7))

# same

qqnorm(rstudent(lm8))
qqline(rstudent(lm8))

# few outliers that are deviating significantly on the very left side

# problem 4.29

data6 <- as.data.frame(read_excel("data-table-B20.xls"))

lm9 <- lm(y ~ ., data=data6)

qqnorm(rstudent(lm9))
qqline(rstudent(lm9))

# some problems with normality. definitely not ideal

Predicted_values = predict(lm9, newdata = data6)
standardized = rstandard(lm9)
plot(Predicted_values, standardized)

# the errors are pretty rangebound actually. equally distributed around 0. We can argue that there are mini clusters in plot

summary(lm9)

# it seems like x1,x2,x3 are significant

lm10 <- lm(y ~ x1 + x2 + x3, data=data6)

qqnorm(rstudent(lm10))
qqline(rstudent(lm10))

# may be slightly better

Predicted_values = predict(lm10, newdata = data6)
standardized = rstandard(lm10)
plot(Predicted_values, standardized)

# not what i was hoping for

plot(data6$x1, data6$y)
plot(data6$x2, data6$y)
plot(data6$x3, data6$y)
plot(data6$x4, data6$y)
plot(data6$x5, data6$y)

# there are too few data points to make any reasonable comments and analyze honestly

#problem 5.2

# part a

temperaturek <- c(273, 283, 293, 303, 313, 323, 333, 343, 353, 363, 373)
vapor <- c(4.6, 9.2, 17.5, 31.8, 55.3, 92.5, 149.4, 233.7, 355.1, 525.8, 760)
dat <- data.frame(temperaturek, vapor)

plot(dat$temperaturek, dat$vapor)

# clearly quadratic relationship

# part b

lm11 <- lm(dat$vapor ~ dat$temperaturek)

summary(lm11)

qqnorm(rstudent(lm11))
qqline(rstudent(lm11))

# not an ideal plot. couple of outliers to the left side

Predicted_values = predict(lm11, newdata = dat)
standardized = rstandard(lm11)
plot(Predicted_values, standardized)

# not equally distributed around 0 (not even close). There is definitely a pattern

# part c

dat$inversetemp <- (2.732)^(-1/dat$temperaturek)

lm12 <- lm(dat$vapor ~ dat$inversetemp)

qqnorm(rstudent(lm12))
qqline(rstudent(lm12))

# OK. there's a terrible outlier. but overall, i think this plot is much better

Predicted_values = predict(lm12, newdata = dat)
standardized = rstandard(lm12)
plot(Predicted_values, standardized)

# the values get closer to 0, but it still doesn't seem like they are equally distributed and there is still a pattern

# problem 5.5

defectsper1000 <- c(13, 16.1, 14.5, 17.8, 22, 27.4, 16.8, 34.2, 65.6, 49.2, 66.2, 81.2, 87.4, 114.5)
weeks <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
dat <- data.frame(defectsper1000, weeks)

lm14 <- lm(dat$defectsper1000 ~ dat$weeks)

qqnorm(rstudent(lm14))
qqline(rstudent(lm14))

# there are outlier to the right and left, but broadly alright

Predicted_values = predict(lm14, newdata = dat)
standardized = rstandard(lm14)
plot(Predicted_values, standardized)

# I think we are quadratic relationship in residuals. log transformation may be

library(MASS)
boxcox(lm14)

# It is pretty clear we need a ln or sqt root transformation

# let me just do a simple scatter plot

plot(dat$weeks, dat$defectsper1000)

# clearly, ln transformation

dat$lndefects <- log(dat$defectsper1000)

lm15 <- lm(dat$lndefects ~ dat$weeks)

qqnorm(rstudent(lm15))
qqline(rstudent(lm15))

#ok

Predicted_values = predict(lm15, newdata = dat)
standardized = rstandard(lm15)
plot(Predicted_values, standardized)

# this plot is much better than last time

# a ln transforamtion is necessary in this case


# problem 5.7

data10 <- as.data.frame(read_excel("data-table-B20.xls"))

lm16 <- lm(y ~ x1 + x2 + x3, data = data10)

qqnorm(rstudent(lm16))
qqline(rstudent(lm16))

# there are few points that are not close to the line especially the ones to the right

Predicted_values = predict(lm16, newdata = data10)
standardized = rstandard(lm16)
plot(Predicted_values, standardized)

# okish. but can be better

boxcox(lm16)

# indicating a ln transformation. let me try that

data10$lny <- log(data10$y)

lm17 <- lm(data10$lny ~ data10$x1 + data10$x2 + data10$x3)

qqnorm(rstudent(lm17))
qqline(rstudent(lm17))

# 1 terrible outlier. But the overall graph is better

Predicted_values = predict(lm17, newdata = data10)
standardized = rstandard(lm17)
plot(Predicted_values, standardized)

# I see a outlier, but the overall graph seems spread out and better

# I would probably do the log transformation