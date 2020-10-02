###9_29 Code, Naive Bayes and Classification and Decision Trees

library(rpart)
library(rpart.plot)

iris
dim(iris) #check dimensions
head(iris)

# creating a sample from the iris dataset     
s_iris <- sample(150, 100)
s_iris
#compare sample when you set a seed vs when you do not set a seed
#will get different data every time when you do not set a seed

#create testing and training sets

iris_train <- iris[s_iris,] #We want the rows from each data in sample set and all the columns
iris_test <- iris[-s_iris,] #use remaining dataset for training by just saying the negative
dim(iris_test)
dim(iris_train)

dectionTreeModel <- rpart(Species~., iris_train, method="class") #~. is just a shortcut instead of saying ~colA+colB+...
dectionTreeModel

rpart.plot(dectionTreeModel)

########################################

abalone <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"), header = FALSE, sep = ",")
summary(abalone)
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings')
str(abalone)

abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings <- as.factor(abalone$rings)

summary(abalone$rings)

#Remove sex because KNN requires all numeric variables

aba <-abalone
aba$sex <-NULL #but base dataframe is left alone

normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

aba[1:7] <-as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_weight)

####################
#Kmeans in IRIS

help("sapply")

iris
dim(iris)
summary(iris)

library(ggplot2)

sapply(iris[,-5], var)
ggplot(iris.aes(x=Petal.Length, y=Petal.Width, col = Species))+geom_point


wss <- sapply(1:k.max, function(k){kmeans(iris[,3:4],k,nstart=20,iter.max=20)$tot.withinss})
wss

plot(1:k.max,wss, type='b', xlab = "number of clusters k", ylab = "Within cluster of sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart=20)
