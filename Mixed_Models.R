#Mixed_Models

library(lme4)
library(lmer)
library(MASS)

politeness <- read.csv("http://www.bodowinter.com/uploads/1/2/9/3/129362560/politeness_data.csv")
head(politeness)


which(is.na(politeness$frequency)) #NA value in line 39

## Including NA because we are using mixed models
##Interesting random effects are in subject and scenario columns
# polite vs. informal attitude
# dependent measure is frequency, continuous variable, representative of pitch


#When designing an experiment, consider factors such as, F might have naturally higher pitch than M
boxplot(frequency~attitude*gender, col=c("white","lightgray"),politeness)

lmer(frequency~attitude, data = politeness) #error, no random effect terms specified in formula
##Add random intercept for subjects and items
##attitude is a single fixed effect

politeness.model <- lmer(frequency~attitude+(1|subject)+(1|scenario), data = politeness)

summary(politeness.model)


###Do OATS dataset example from .ppt 12/8



###Fitting a Regression Tree
library(MASS)
library(tree)
set.seed(12345)
help("Boston")
head(Boston)
## Check dimension and structure of data first
train= sample(1:nrow(Boston), nrow(Boston)/2) #training data is randomly separated from the total data
tree.boston = tree(medv ~., Boston, subset = train)
summary(tree.boston) #stars are leaf nodes



#Create Regression Tree
tree(formula = medv ~., data=Boston, subset=train)
#medv = median value of owner-occupied homes per thousand dollars

plot(tree.boston)
text(tree.boston, pretty = 0)
#look at the most important variable (first split)


cv.boston=cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, typ='b')





help("prune.tree") #snips off least important splits
prune.boston = prune.tree(tree.boston, best = 5)

plot(prune.boston)
text(prune.boston, pretty=0)

yhat = predict(tree.boston, newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
mean((yhat-boston.test)^2) #very high error
###Bagging and Random Forest example
library(randomForest)
set.seed(12345)
#Use mtry=13 to indicate all 13 predictors should be considered for each split of the tree, i.e. bagging should be done (this is bagging, using randomForest)
bag.boston = randomForest(medv ~., data= Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston

# Test bagged model performance on the test set
yhat.bag = predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
mean((yhat.bag-boston.test)^2) #much better error with bagging


#Change number of trees grown by randomForest
set.seed(12345)
rf.boston=randomForest(medv~., data=Boston, subset = train, mtry=6, importance = TRUE)
yhat.rf = predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2) #even lower error, random forest > bagging in this case

##Importance of each variable
importance(rf.boston)
#Col1 is mean decrease of accuracy in predictions on out of bag samples when a given variable is exlcuded from the model (i.e., if other 12 are compared)
#Col2 is measure of total decrease in node impurity that results from splits over that variable, averaged over all trees
#node impurity is the training regression sum of squares for regression trees, and is deviance for classification trees
#Plots using varImpPlot()
varImpPlot(rf.boston)

##Use caret for iris dataset
library(caret)
data(iris)
dataset <- iris
#use 80% of data for training
validation_index <- createDataPartition(dataset$Species, p = 0.80, list = FALSE)


#select 20% of this separated data for validation

validation <- dataset[-validation_index,]

#Use 80% of separated data (80% of 80%, i.e. 64% of total data)
dataset <- dataset[validation_index,]
dim(dataset)

sapply(dataset,class)
head(dataset)
levels(dataset$Species)
percentage <- prop.table(table(dataset$Species))*100
cbind(freq = table(dataset$Species), percentage = percentage)


x <- dataset[,]