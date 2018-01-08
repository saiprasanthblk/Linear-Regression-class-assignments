# Assignment 5

# 6.12, 6.13, 6.14, 6.15, 8.5, 8.6, 8.13, 8.14

library(readxl)

# problem 6.12

data <- as.data.frame(read_excel("data-table-B11.xls"))

model <- lm(data$Quality ~ data$Clarity + data$Aroma + data$Body + data$Flavor + data$Oakiness)

qqnorm(rstudent(model))
qqline(rstudent(model))

# we see that there are quite a few points that are below the line towards the right end and left end but all the points expect one point lie between -2 and 2

ti<-rstudent(model)

yhat <- fitted(model)
plot(yhat,ti)

# yep. We see a point beyond -2 to 2 range.


# lets plot against the predictors

plot(data$Clarity,ti) # one point beyond -2 to 2 range

plot(data$Aroma,ti) # one point beyond -2 to 2 range

plot(data$Body,ti) # one point beyond -2 to 2 range

plot(data$Flavor,ti) # one point beyond -2 to 2 range

plot(data$Oakiness,ti) # one point beyond -2 to 2 range



#Cooks distance

cooks.distance(model)

# Dfbetas

dfbetas(model)

# dffits

dffits(model)

# covratio

covratio(model)

summary(influence.measures(model))

# my guess is point 20 is the influential point



# problem 6.13

data <- as.data.frame(read_excel("data-table-B12.xls"))

model <- lm(data$pitch ~ ., data = data)

qqnorm(rstudent(model))
qqline(rstudent(model))

# There is certainly a outlier. There could be two. Lets see

ti<-rstudent(model)

yhat <- fitted(model)
plot(yhat,ti)

# same comment


# lets plot against the predictors

plot(data$temp,ti) # one outlier. there's a point at two

plot(data$soaktime,ti) # one point beyond -2 to 2 range

plot(data$Soakpct,ti) # one point beyond -2 to 2 range

plot(data$difftime,ti) # one point beyond -2 to 2 range

plot(data$diffpct,ti) # one point beyond -2 to 2 range


#Cooks distance

cooks.distance(model)

# Dfbetas

dfbetas(model)

# dffits

dffits(model)

# covratio

covratio(model)

summary(influence.measures(model))

# I think point 32 is most certainly an influential point. There are points which have starts associated with the measures but points 32 measure values are much larger than values for other points


# problem 6.14

data <- as.data.frame(read_excel("data-table-B13.xls"))

model <- lm(data$y ~ ., data = data)

qqnorm(rstudent(model))
qqline(rstudent(model))

# Seems like there's two outliers

ti<-rstudent(model)

yhat <- fitted(model)
plot(yhat,ti)

# There is certainly a outlier. But there are quite a few points around the 2 and -2 boundary


# lets plot against the predictors

plot(data$x1,ti) # one outlier. many points close to boundary

plot(data$x2,ti) # one outlier. many points close to boundary

plot(data$x3,ti) # one outlier. many points close to boundary

plot(data$x4,ti) # one outlier. many points close to boundary

plot(data$x5,ti) # one outlier. many points close to boundary

plot(data$x6,ti) # one outlier. many points close to boundary


#Cooks distance

cooks.distance(model)

# Dfbetas

dfbetas(model)

# dffits

dffits(model)

# covratio

covratio(model)

summary(influence.measures(model))

# I think point 20 is that one point that is massively out of boundary range (the cooks distance is also massive). Clearly there are other points that are reasonably inflencing too.



# problem 6.15

data <- as.data.frame(read_excel("data-table-B14.xls"))

model <- lm(data$y ~ data$x1 + data$x2 + data$x3 + data$x4)

qqnorm(rstudent(model))
qqline(rstudent(model))

# Few points close to 2. Not sure if they are massively influential

ti<-rstudent(model)

yhat <- fitted(model)
plot(yhat,ti)

# Few points close to 2. Not sure if they are massively influential


# lets plot against the predictors

plot(data$x1,ti) # Few points close to 2. Not sure if they are massively influential

plot(data$x2,ti) # Few points close to 2. Not sure if they are massively influential

plot(data$x3,ti) # Few points close to 2. Not sure if they are massively influential

plot(data$x4,ti) # Few points close to 2. Not sure if they are massively influential

#Cooks distance

cooks.distance(model)

# Dfbetas

dfbetas(model)

# dffits

dffits(model)

# covratio

covratio(model)

summary(influence.measures(model))

# I think we could say 2,4, (may be 8 and 10) are influential points.


# problem 8.5

data <- as.data.frame(read_excel("data-table-B3.xls"))

model <- lm(data$y ~ data$x10 + data$x11)

summary(model)

# doesn't seem like x11 is significant

model2 <- lm(data$y ~ data$x10 + data$x11 + (data$x10*data$x11))

summary(model2)

# Now all the 3 terms are significant

# when the transmission is automatic, the equation is y = 31.9 - (0.0035)X10

# when the transmission is not manual, the equation is y = 58.1- (0.0125)X10


# problem 8.6

data <- as.data.frame(read_excel("data-table-B1.xls"))

model7 <- lm(data$y ~ x7 + x8 + I(data$x5 > 0) + I(data$x5 <0), data = data)

summary(model7)

anova(model7)

# not significant


# problem 8.13

data <- as.data.frame(read_excel("data-table-B11.xls"))

data$Region = as.factor(data$Region)

model <- lm(Quality ~ Flavor + Region, data = data)

summary(model)

anova(model) 

# significant

qqnorm(rstudent(model))
qqline(rstudent(model))

# There are certainly some points beyond -2 to 2

ti<-rstudent(model)

yhat <- fitted(model)
plot(yhat,ti)

summary(influence.measures(model))

# There are some points very close to 2 and 2, but broadly the variance seems constant, equally distributed around 0

# There are certainly some influential points

model2 <- lm(Quality ~ Flavor + Region + (Flavor*Region), data = data)

summary(model2)

qqnorm(rstudent(model2))
qqline(rstudent(model2))


ti<-rstudent(model2)

yhat <- fitted(model2)
plot(yhat,ti)

summary(influence.measures(model2))

# I don't think this ones any better than the last one. Moreover now variance slightly varies in residual plot

# problem 8.14

data <- as.data.frame(read_excel("data-table-B11.xls"))

data$Region = as.numeric(data$Region)

model <- lm(Quality ~ Flavor + Region, data = data)

summary(model)

anova(model) 

# The model in 8.13 is much better



