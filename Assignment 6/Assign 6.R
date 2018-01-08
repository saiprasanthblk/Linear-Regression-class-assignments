#Reading: Chapters 9, 10, and 11

#Problems (all using R): 9.7, 9.13, 9.14, 9.19, 9.20, 10.1, 10.2, 10.14, 10.15, 10.16

#Note: Problem 10.15 refers to problem 10.14, not problem 10.4. 

library(readxl)

# 9.7

# part a

mileage <- as.data.frame(read_excel("data-table-B3.xls"))

cor(mileage)

# there are definitely some variables with high correlation

# part b

model <- lm(y ~ ., data = mileage)

library(car)

vif(model)

# There is certainly a multi-collinearity problem in this data set


# 9.13

fuel <- as.data.frame(read_excel("data-table-B18.xls"))
cor(fuel)
# there are definitely some variables with high correlation

# part b
model <- lm(y ~ ., data = fuel)
vif(model)
# There is certainly a multi-collinearity problem in this data set. Some of the VIF's are in hundreds here


# 9.14

wine <- as.data.frame(read_excel("data-table-B19.xls"))
cor(wine)
# I do see some values above 0.9

# part b
model <- lm(y ~ ., data = wine)
vif(model)
# The VIF doesn't even run beacuse one of the variables is perfectly correlated with another variable!


# 9.19

# part a

library(tidyverse)
y<- mileage$y
x<- mileage %>% select(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11) %>% data.matrix()
lambdas <- 10^seq(3, -2, by = -.1)
cv_fit <- cv.glmnet(x, y, alpha = 0, lambda = lambdas)
plot(cv_fit)
opt_lambda <- cv_fit$lambda.min
opt_lambda
# 5.011872

# part b

simple_linear_model <- lm(y~., data = mileage)

sum(simple_linear_model$residuals^2)

# a linear model sum of squares gives 201


fit <- cv_fit$glmnet.fit
summary(fit)
y_predicted <- predict(fit, s = opt_lambda, newx = x)

# Sum of Squares Total and Error
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

sse

# 299

# Hence SSE has increased from 200 to 299 actually


# part c

# R squared
rsq <- 1 - sse / sst
rsq 

# 75% r square for ridge regression

summary(simple_linear_model) 

# 75% r square (adjusted) for the simple linear model. Hence, pretty much the same R square



# 9.20





# 10.1

library(leaps)

sdata <- as.data.frame(read_excel("data-table-B1.xls"))
# Iterative model selection
# Begin by defining the models with no variables (null) and all variables (full)
ten.null <- lm(y~1, data=sdata)
ten.full <- lm(y~., data=sdata)

## Forward selection
step(ten.null, scope=list(lower=ten.null, upper=ten.full), direction="forward")

## Backward selection
step(ten.full, scope=list(lower=ten.null, upper=ten.full), direction="backward")

## Stepwise selection
step(ten.null, scope=list(lower=ten.null, upper=ten.full), direction="both")

# all the 3 gave the same model

# 10.2

sdata <- sdata[, c(1, 2, 4, 7, 8, 9)]

library(leaps)
bestmod <- regsubsets(y~., data=sdata, nbest=10)

## The 10 best models for each number of explanatory variables in the model
summary(bestmod)
best.sum <- as.data.frame(summary(bestmod)$outmat)
best.sum$p <- as.numeric(substr(rownames(best.sum),1,1))+1

## The criterion values corresponding to each model
best.sum$rss <- summary(bestmod)$rss
best.sum$adjr2 <- summary(bestmod)$adjr2
best.sum$cp <- summary(bestmod)$cp
best.sum$bic <- summary(bestmod)$bic

## Determine "best" models
best.sum[order(best.sum$rss),]
best.sum[order(best.sum$adjr2),]
best.sum[order(best.sum$cp),]
best.sum[order(best.sum$bic),]

# I would recommend x2, x7 and x8



# 10.14

wine_pinot <- as.data.frame(read_excel("data-table-B11.xls"))

wine_pinot$Region1 <- ifelse(wine_pinot$Region==1, 1, 0)
wine_pinot$Region2 <- ifelse(wine_pinot$Region==2, 1, 0)
wine_pinot$Region3 <- ifelse(wine_pinot$Region==3, 1, 0)

bestmod <- regsubsets(Quality ~ ., data = wine_pinot[,-7], nbest=10)

summary(bestmod)
best.sum <- as.data.frame(summary(bestmod)$outmat)
best.sum$p <- as.numeric(substr(rownames(best.sum),1,1))+1

best.sum$cp <- summary(bestmod)$cp

best.sum[order(best.sum$cp),]

# best modek is flavor, oakiness, region1, region2
# 2nd best model is flavor, oakiness, region2 and region 3

# both are pretty close

model1 <- lm(Quality ~ Flavor + Oakiness + Region, data = wine_pinot)

qqnorm(rstudent(model1))
qqline(rstudent(model1))

r <- resid(model1)
pr <- resid(model1)/(1 - lm.influence(model1)$hat)
sum(r^2)
press<- sum(pr^2)
press


# 10.15

sdata <- wine_pinot[,-7]

# Iterative model selection
# Begin by defining the models with no variables (null) and all variables (full)

ten.null <- lm(y~1, data=sdata)
ten.full <- lm(y~., data=sdata)

## Forward selection
step(ten.null, scope=list(lower=ten.null, upper=ten.full), direction="forward")

## Backward selection
step(ten.full, scope=list(lower=ten.null, upper=ten.full), direction="backward")

## Stepwise selection
step(ten.null, scope=list(lower=ten.null, upper=ten.full), direction="both")



# 10.16

bestmod <- regsubsets(Quality ~ ., data = wine_pinot[,-7, -8, -9, -10], nbest=10)

summary(bestmod)
best.sum <- as.data.frame(summary(bestmod)$outmat)
best.sum$p <- as.numeric(substr(rownames(best.sum),1,1))+1

best.sum$cp <- summary(bestmod)$cp

best.sum[order(best.sum$cp),]



















