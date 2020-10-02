library(ggplot2)
library(rpart)
library(rpart.plot)
data("msleep")
str(msleep)
help("msleep")
str(data)


mSleepDF1 <- msleep[,c(3,6,10,11)]
str(mSleepDF1)
head(mSleepDF1)


help("rpart")

sleepModel_1 <- rpart(sleep_total ~ ., data=mSleepDF1, method = 'anova')
sleepModel_1

help("rpart.plot")

rpart.plot(sleepModel_1, type = 3, fallen.leaves = TRUE)

rpart.plot(sleepModel_1, type = 3, digits = 4, fallen.leaves = TRUE)
rpart.plot(sleepModel_1, type = 3, digits = 4, fallen.leaves = TRUE)

######### Iris Dataset #########
require(C50)
data("iris")
head(iris)
str(iris)
table(iris$Species)

set.seed(9850)
grn<-runif(nrow(iris))

#creating a randomized iris dataset, shuffling the dataset
#we use order() function along with the random numbers we generated
irisrand <- iris[order(grn),]

str(irisrand)
classificationmodel1 <- C5.0(irisrand[1:100,-5],irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)

prediction1 <- predict(classificationmodel1,irisrand[101:150,])
prediction1


#confusion matrix to understand prediction data

table(irisrand[101:150,5], prediction1)
plot(classificationmodel1)



library("e1071")
classifier<-naiveBayes(iris[,1:4], iris[,5])
table(predict(classifier, iris[,-5]), iris[,5], dnn=list('predicted','actual'))
classifier$apriori
classifier$tables$Petal.Length
plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species")
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue")
curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")



###Rpart 1
require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart) # try some different plot options
text(swiss_rpart) # try some different text options

require(party)

treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))
# look at help info, vary parameters.

library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)
#find "prettier" ways to plot the tree

###Rpart 2

# Conditional Inference Tree for Mileage
fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
# plot tree
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")
text(fit2M, use.n=TRUE, all=TRUE, cex=.8)

###Rpart 3

fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")
