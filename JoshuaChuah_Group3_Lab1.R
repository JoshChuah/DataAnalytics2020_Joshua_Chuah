####Lab 3 Scripts


set.seed(12345)
help(par)

par(mar = rep(0.2,4))
data_Matrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])


#make heatmap
par(mar = rep(0.2,4))
heatmap(data_Matrix)

set.seed(678910)
for(i in 1:40){
  coin_Flip <- rbinom(1,size=1,prob=0.5)
  
  if(coin_Flip){
    data_Matrix[i,] <- data_Matrix[i,]+rep(c(0,3),each=5)
  }
}

par(mar = rep(0.2,4))
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])

par(mar = rep(0.2,4))
heatmap(data_Matrix)

hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1,xlab="The Row Mean", ylab = "Row", pch = 19)
plot(colMeans(data_Matrix_Ordered), xlab="Column", ylab="Column Mean", pch = 19)

###Titanic Data

library(titanic)
data(Titanic)
require(titanic)
attach(titanic_train)
titanic_train <- na.omit(titanic_train)
Survived <- titanic_train$Survived
survived_rpart <- rpart(Survived ~ Age + Pclass + SibSp + Sex)
plot(survived_rpart)
text(survived_rpart)


treeSurvived <- ctree(Survived ~ Age + Pclass+ Fare+SibSp, data=titanic_train)
plot(treeSurvived)

t <- dist(as.matrix(na.omit(titanic_train)))
hc <- hclust(t)
plot(hc)

library(randomForest)
fitSurvived <- randomForest(Survived~ Age + Pclass + Fare + SibSp, data = titanic_train)
print(fitSurvived)
importance(fitSurvived)

#############Lab1 Examples Below


#### Lab 1 Ctree1

require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart) # try some different plot options
text(swiss_rpart) # try some different text options

require(party)
library(partykit)
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

###Lab 1 Ctree2

# Conditional Inference Tree for Mileage
fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
# plot tree
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")
text(fit2M, use.n=TRUE, all=TRUE, cex=.8)

###Lab 1 Ctree3
fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")


###Lab 1 Dr1
library(MetaPCA)
#Spellman, 1998 Yeast cell cycle data set
#Consider each synchronization method as a separate data
data(Spellman) 
pc <- list(alpha=prcomp(t(Spellman$alpha))$x, cdc15=prcomp(t(Spellman$cdc15))$x,
			cdc28=prcomp(t(Spellman$cdc28))$x, elu=prcomp(t(Spellman$elu))$x)
#There are currently 4 meta-pca methods. Run either one of following four.
metaPC <- MetaPCA(Spellman, method="Eigen", doPreprocess=FALSE)
plot
metaPC <- MetaPCA(Spellman, method="RobustAngle", doPreprocess=FALSE)
metaPC <- MetaPCA(Spellman, method="SparseAngle", doPreprocess=FALSE)
#Comparing between usual pca and meta-pca
#The first lows are four data sets based on usual PCA, and 
#the second rows are by MetaPCA
#We're looking for a cyclic pattern.
par(mfrow=c(2,4), cex=1, mar=c(0.2,0.2,0.2,0.2))
for(i in 1:4) {
		plot(pc[[i]][,1], pc[[i]][,2], type="n", xlab="", ylab="", xaxt="n", yaxt="n")
		text(pc[[i]][,1], pc[[i]][,2], 1:nrow(pc[[i]]), cex=1.5)
		lines(pc[[i]][,1], pc[[i]][,2])
}
for(i in 1:4) {
		plot(metaPC$x[[i]]$coord[,1], metaPC$x[[i]]$coord[,2], type="n", xlab="", ylab="", xaxt="n", yaxt="n")
		text(metaPC$x[[i]]$coord[,1], metaPC$x[[i]]$coord[,2], 1:nrow(metaPC$x[[i]]$coord), cex=1.5)
		lines(metaPC$x[[i]]$coord[,1], metaPC$x[[i]]$coord[,2])
}

metaPC <- MetaPCA(prostate, method="Eigen", doPreprocess=FALSE, .scale=TRUE)
metaPC <- MetaPCA(prostate, method="Angle", doPreprocess=FALSE)
metaPC <- MetaPCA(prostate, method="RobustAngle", doPreprocess=FALSE)
metaPC <- MetaPCA(prostate, method="SparseAngle", doPreprocess=FALSE,iter=100)
#Plotting 4 data in the same space!
coord <- foreach(dd=iter(metaPC$x), .combine=rbind) %do% dd$coord
PlotPC2D(coord[,1:2], drawEllipse=F, dataset.name="Prostate", .class.order=c("Metastasis","Primary","Normal"), 
         .class.color=c('red','#838383','blue'), .annotation=T, newPlot=T,
         .class2=rep(names(metaPC$x), times=sapply(metaPC$x,function(x)nrow(x$coord))), 
         .class2.order=names(metaPC$x), .points.size=1)

#In the case of "SparseAngle" method, the top contributing genes for all studies can be determined
#For instance, top 20 genes in 1st PC and their coefficients
metaPC$v[order(abs(metaPC$v[,1]), decreasing=TRUE),1][1:20] 


library(EDR)
demo(edr_ex1)
demo(edr_ex2)
demo(edr_ex3)
demo(edr_ex4)


library(dr)
data(ais)
# default fitting method is "sir"
s0 <- dr(LBM~log(SSF)+log(Wt)+log(Hg)+log(Ht)+log(WCC)+log(RCC)+
           log(Hc)+log(Ferr),data=ais) 
# Refit, using a different function for slicing to agree with arc.
summary(s1 <- update(s0,slice.function=dr.slices.arc))
# Refit again, using save, with 10 slices; the default is max(8,ncol+3)
summary(s2<-update(s1,nslices=10,method="save"))
# Refit, using phdres.  Tests are different for phd, and not
# Fit using phdres; output is similar for phdy, but tests are not justifiable. 
summary(s3<- update(s1,method="phdres"))
# fit using ire:
summary(s4 <- update(s1,method="ire"))
# fit using Sex as a grouping variable.  
s5 <- update(s4,group=~Sex)
