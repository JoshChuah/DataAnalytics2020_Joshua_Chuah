##SVM_InClass 

library(ggplot2)
library(e1071)
data("iris")
head(iris)
attach(iris)
qqplot(Petal.Length, Petal.Width, data=iris, color=Species)

svm_model1 <- svm(Species~., data=iris)
summary(svm_model1)

pred1 <- predict(svm_model1, iris)
table1 <- table(Predicted = pred1, Actual = iris$Species)
table1

svm_model3 <- svm(Species~., data=iris, kernel='polynomial')
summary(svm_model3)
pred3 <- predict(svm_model3, iris)
table3 <- table(Predicted = pred3, Actual = iris$Species)
table3


