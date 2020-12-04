#10/27 Lecture Codes

library(ISLR)
library(MASS)
library(boot)
set.seed(1)


help("sample") #takes sample from elements of x of size n
train = sample(392,196) #we set the seed, so this will be a vector of random elements

lm.fit <- lm(mpg~horsepower, data = Auto, subset = train) #linear fit model between mpg and horsepower data from the dataframe Auto
#subset used to only using observations from training set

#use predict function to estimate response for all 392 observations
#mean() for MSE of 196 observations in validation set

#sample(training#,validation#) when we have a set seed

attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2) #i.e., find MSE for all predictions for data not in the training set

#Quadratic Regression line
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

#Cubic Regression Line
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

#Quadratic better than cubic, complex data might be creating overfitting


#Different seed

set.seed(2)
train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# the error rate is 23.29
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic 
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 
# the error rate is 18.90
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)



