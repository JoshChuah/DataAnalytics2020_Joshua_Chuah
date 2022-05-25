library(gdata)
bronx1 <- read.xls(file.choose(), pattern="BOROUGH", stringAsFactors=FALSE, sheet=1, per1="<SOMEWHERE>/perl/bin/per1.exe")
bronx1 <- bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]

#alternate
#library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#bronx1<-read.xlsx("<SOMEWHERE>/rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheetIndex=1,startRow=5,header=TRUE)
View(bronx1)
#
attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 
m1<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

m2<-lm(log(bronx1$SALE.PRICE)~log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))

bronx1$SALE.PRICE<-sub("\\$","",bronx1$SALE.PRICE) 
bronx1$SALE.PRICE<-as.numeric(gsub(",","", bronx1$SALE.PRICE)) 
bronx1$GROSS.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$GROSS.SQUARE.FEET)) 
bronx1$LAND.SQUARE.FEET<-as.numeric(gsub(",","", bronx1$LAND.SQUARE.FEET)) 
bronx1$SALE.DATE<- as.Date(gsub("[^]:digit:]]","",bronx1$SALE.DATE)) 
bronx1$YEAR.BUILT<- as.numeric(gsub("[^]:digit:]]","",bronx1$YEAR.BUILT)) 
bronx1$ZIP.CODE<- as.character(gsub("[^]:digit:]]","",bronx1$ZIP.CODE)) 

minprice<-10000
bronx1<-bronx1[which(bronx1$SALE.PRICE>=minprice),]
nval<-dim(bronx1)[1]

bronx1$ADDRESSONLY<- gsub("[,][[:print:]]*","",gsub("[ ]+","",trim(bronx1$ADDRESS))) bronxadd<-unique(data.frame(bronx1$ADDRESSONLY, bronx1$ZIP.CODE,stringsAsFactors=FALSE)) names(bronxadd)<-c("ADDRESSONLY","ZIP.CODE") bronxadd<-bronxadd[order(bronxadd$ADDRESSONLY),] duplicates<-duplicated(bronx1$ADDRESSONLY)

for(i in 1:2345) {
  if(duplicates[i]==FALSE) dupadd<-bronxadd[bronxadd$duplicates,1]
}#what are we doing with dupadd?

nsample=450

addsample<-bronxadd[sample.int(dim(bronxadd),size=nsample),]#I use nval here 
# may need to install this package
library(ggmap)
addrlist<-paste(addsample$ADDRESSONLY, "NY", addsample$ZIP.CODE, "US", sep=" ") 
querylist<-geocode(addrlist) #This is cool. Take a break.

matched<-(querylist$lat!=0 &&querylist$lon!=0) addsample<-cbind(addsample,querylist$lat,querylist$lon) 
names(addsample)<-c("ADDRESSONLY","ZIPCODE","Latitude","Longitude")# correct the column na adduse<-merge(bronx1,addsample)

adduse<-adduse[!is.na(adduse$Latitude),]
mapcoord<-adduse[,c(2,3,24,25)]

table(mapcoord$NEIGHBORHOOD)

mapcoord$NEIGHBORHOOD <- as.factor(mapcoord$NEIGHBORHOOD)
map <- get_map(location = 'Bronx', zoom = 12)#Zoom 11 or 12
ggmap(map) + geom_point(aes(x = mapcoord$Longitude, y = mapcoord$Latitude, size =1, color=mapcoord$NEIGHBORHOOD), data = mapcoord) +theme(legend.position = "none") 

#It would be perfect if I can decrease the size of points 

mapmeans<-cbind(adduse,as.numeric(mapcoord$NEIGHBORHOOD))
colnames(mapmeans)[26] <- "NEIGHBORHOOD" #This is the right way of renaming.

keeps <- c("ZIP.CODE","NEIGHBORHOOD","TOTAL.UNITS","LAND.SQUARE.FEET","GROSS.SQUARE.FEET","SALE.PRICE","Latitude","Longitude") 
mapmeans<-mapmeans[keeps]#Dropping others
mapmeans$NEIGHBORHOOD<-as.numeric(mapcoord$NEIGHBORHOOD) 

for(i in 1:8){
  mapmeans[,i]=as.numeric(mapmeans[,i]) 
}#Now done for conversion to numeric

#Classification
mapcoord$class<as.numeric(mapcoord$NEIGHBORHOOD)
nclass<-dim(mapcoord)[1]
split<-0.8
trainid<-sample.int(nclass,floor(split*nclass))
testid<-(1:nclass)[-trainid]

##mappred<-mapcoord[testid,] # What would you use this for?
##mappred$class<as.numeric(mappred$NEIGHBORHOOD) 

kmax<-10
knnpred<-matrix(NA,ncol=kmax,nrow=length(testid))
knntesterr<-rep(NA,times=kmax)
for (i in 1:kmax){		# loop over k
  knnpred[,i]<-knn(mapcoord[trainid,3:4],mapcoord[testid,3:4],cl=mapcoord[trainid,2],k=i)
  knntesterr[i]<-sum(knnpred[,i]!=mapcoord[testid,2])/length(testid)
} 
knntesterr

#Clustering
mapobj<-kmeans(mapmeans,5, iter.max=10, nstart=5, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobj,method=c("centers","classes"))
mapobj$centers
#
library(cluster)
clusplot(mapmeans, mapobj$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
#
library(fpc)#May need to install.packages("fpc")
plotcluster(mapmeans, mapobj$cluster)
#
mapmeans1<-mapmeans[,-c(1,3,4)]
mapobjnew<-kmeans(mapmeans1,5, iter.max=10, nstart=5, algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
fitted(mapobjnew,method=c("centers","classes"))
clusplot(mapmeans1, mapobjnew$cluster, color=TRUE, shade=TRUE, labels=2, lines=0) 
plotcluster(mapmeans1, mapobjnew$cluster)
ggmap(map) + geom_point(aes(x = mapcoord$Longitude, y = mapcoord$Latitude, size =1, color=mapobjnew$cluster), data = mapcoord)#How to change colors?




#######

pairs(Fertility ~ Agriculture + Education + Catholic, data = swiss, subset = Education < 20, main = "Swiss data, Education < 20")
require(party)
swiss_ctree <- ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_ctree)



allexamples <- FALSE

y<- data.frame(A=c(rep("red",100), rep("blue", 100)), B=c(rnorm(100),round(rnorm(100,5,1),1)), C=runif(200),
               D=c(rep("big", 150), rep("small", 50)),
               E=rnorm(200))
library(gpairs)
gpairs(y)

data(iris)
gpairs(iris)

if(allexamples){
  gpairs(iris, upper.pars = list(scatter = 'stats'), scatter.aprs = list(pch = substr(as.character(iris$Species), 1, 1), col = as.numeric(iris$Species)),stat.pars = list(verbose=FALSE))
  gpairs(iris, lower.pars = list(scatter = 'corrgram'),
         upper.pars = list(conditional = 'boxplot', scatter = 'loess'),
         scatter.pars = list(pch = 20))
}

data(Leaves)
gpairs(Leaves[1:10], lower.pars = list(scatter= 'loess'))
if (allexamples) {
  gpairs(Leaves[1:10], upper.pars = list(scatter = 'stats'),
         lower.pars = list(scatter = 'corrgram'),
         stat.pars = list(verbose = FALSE), gap = 0)
  corrgram(Leaves[,-33])
}
runexample <- FALSE
if (runexample) {
  data(NewHavenResidential)
  gpairs(NewHavenResidential)
}