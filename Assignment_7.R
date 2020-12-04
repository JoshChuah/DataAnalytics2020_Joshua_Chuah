library(ggplot2)
Cancer <- read.csv("cervical_cancer_behavior_risk.csv", header=TRUE)

cancer_pos <- Cancer[Cancer$ca_cervix==1,]
cancer_neg <- Cancer[Cancer$ca_cervix==0,]

####How does behavior influence Cervical Cancer Risk?

### Exploratory data analytics
##SexRisk
#########
sex_pos <- cancer_pos$behavior_sexualRisk
sex_neg <- cancer_neg$behavior_sexualRisk


a<- data.frame(group = "Sex Pos", value = sex_pos)
b <- data.frame(group = "Sex Neg", value = sex_neg)

plot.data <- rbind(a,b)
ggplot(plot.data, aes(x=group, y=value, fill=group)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=14, color="red", fill="red")

hga <- hist(sex_pos,plot = FALSE)
hgb <- hist(sex_neg, plot=FALSE)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

plot(hga, col=c1)
plot(hgb, col=c2, add=TRUE)


########
###Diet
########


eat_pos <- cancer_pos$behavior_eating
eat_neg <- cancer_neg$behavior_eating

a<- data.frame(group = "Eat Pos", value = eat_pos)
b <- data.frame(group = "Eat Neg", value = eat_neg)

plot.data <- rbind(a,b)
ggplot(plot.data, aes(x=group, y=value, fill=group)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=14, color="red", fill="red")

hga <- hist(eat_pos,plot = FALSE)
hgb <- hist(eat_neg, plot=FALSE)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

plot(hgb, col=c2)
plot(hga, col=c1, add=TRUE)


Behavior <- Cancer[,c(1,2,3,20)]

attach(Behavior)

#####Naive Bayes Model
require(mlbench)
library(e1071)

Behavior$ca_cervix <- factor(Behavior$ca_cervix, levels=c(0,1), labels=c("Negative","Positive"))
model <- naiveBayes(ca_cervix ~., data=Behavior)
predict(model, Behavior[,-4])
table(predict(model, Behavior[,-4]), Behavior[,4])

#data(iris)
#m <- naiveBayes(Species ~ ., data = iris)
#m
#predict(m, iris[,-5])
#table(predict(m, iris[,-5]), iris[,5])


#####Logistic Regression Model
Behavior <- Cancer[,c(1,2,3,20)]
n <- dim(Behavior)[1]
training.samples <- sample(1:n, size = round(2*n/3), replace = FALSE, 
                           prob = rep(1/n, n)) 
train.data <- Behavior[training.samples,]
test.data <- Behavior[-training.samples,]
model2 <- glm(ca_cervix~., data=train.data, family=binomial)



### Cross Validation with 6 partitions
cv.err <- cv.glm(train.data, model2)$delta
cv.err


###LOOCV
muhat <- fitted(model2)
muhat
behavior.diag <- glm.diag(model2)
cv.esterr <- mean((model2$y-muhat)^2/(1-behavior.diag$h)^2)
cv.esterr
#####What do we do with cross-validation?

summary(model2)
model2$coefficients
model2$residuals
library(dplyr)
probabilities <- model2 %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

#Model accuracy
mean(predicted.classes==test.data$ca_cervix)



###SVM

svm.model <- svm(ca_cervix ~., data=train.data, cost=1000, gamma=0.0001)
svm.pred <- predict(svm.model, test.data[,-4])

### SVM Model Error
crossprod(svm.pred - test.data[,4]) / length(test.data$behavior_sexualRisk)
