#####CleanEDA


####Load in Data and Separate By Indicator
EDA <- read.csv("finalEDA.csv", header = TRUE)

ANC <- EDA[EDA$Indicator == "Antenatal care 1+ visit - percentage of women (aged 15-49 years) attended at least once during pregnancy by skilled health personnel",]
IB <- EDA[EDA$Indicator == "Institutional deliveries - percentage of deliveries in a health facility",]
NMR <- EDA[EDA$Indicator == "Neonatal mortality rate",]
WAT <- EDA[EDA$Indicator == "Proportion of population using improved drinking water sources",]
SANI <- EDA[EDA$Indicator == "Proportion of population using improved sanitation facilities",]
SHP <- read.csv("SHP_2014.csv", header = TRUE)
LBW <- EDA[EDA$Indicator == "Prevalence of low birth weight among new-borns",]


GDP <- read.csv("GDP.csv", header=TRUE, skip=4)
f_GDP <- GDP[GDP$Country.Name %in% intersection,]
df_GDP <- data.frame("Country"=f_GDP$Country.Name, "Value" = f_GDP$X2014)
df_GDP <- df_GDP[order(df_GDP$Country),]
df_GDP
#######
##Hypothesis Testing
#######
over <- NMR[NMR$OBS_VALUE > 12,]$Geographic.area
under <- NMR[NMR$OBS_VALUE <= 12,]$Geographic.area

over_ANC <- ANC[ANC$Geographic.area %in% over,]$OBS_VALUE
under_ANC <- ANC[ANC$Geographic.area %in% under,]$OBS_VALUE
t.test(over_ANC, under_ANC)
wilcox.test(over_ANC, under_ANC)

over_IB <- IB[IB$Geographic.area %in% over,]$OBS_VALUE
under_IB <- IB[IB$Geographic.area %in% under,]$OBS_VALUE
t.test(over_IB, under_IB)
wilcox.test(over_IB, under_IB)

over_WAT <- WAT[WAT$Geographic.area %in% over,]$OBS_VALUE
under_WAT <- WAT[WAT$Geographic.area %in% under,]$OBS_VALUE
t.test(over_WAT, under_WAT)
wilcox.test(over_WAT, under_WAT)

over_SANI <- SANI[SANI$Geographic.area %in% over,]$OBS_VALUE
under_SANI <- SANI[SANI$Geographic.area %in% under,]$OBS_VALUE
t.test(over_SANI, under_SANI)
wilcox.test(over_SANI, under_SANI)

over_LBW <- LBW[LBW$Geographic.area %in% over,]$OBS_VALUE
under_LBW <- LBW[LBW$Geographic.area %in% under,]$OBS_VALUE
t.test(over_LBW, under_LBW)
wilcox.test(over_LBW, under_LBW)

over_SHP <- SHP[SHP$Country %in% over,]$Births.attended.by.skilled.health.personnel....
under_SHP<- SHP[SHP$Country %in% under,]$Births.attended.by.skilled.health.personnel....
t.test(over_SHP, under_SHP)
wilcox.test(over_SHP, under_SHP)

over_GDP <- GDP[GDP$Country.Name %in% over,]$X2014
under_GDP <- GDP[GDP$Country.Name %in% under,]$X2014
under_GDP <- under_GDP[under_GDP < 150000]
t.test(over_GDP, under_GDP)
wilcox.test(over_GDP, under_GDP)

library(ggplot2)

#########
####Using Full Model First (all variables)
#########
intersection <- Reduce(intersect, list(ANC$Geographic.area, IB$Geographic.area, WAT$Geographic.area, SANI$Geographic.area, LBW$Geographic.area, GDP$Country.Name, SHP$Country))
NMR_int <- NMR[NMR$Geographic.area %in% intersection,]
full_labels <- vector(mode="character", length=length(intersection))
for(val in 1:length(full_labels)){
  if(NMR_int$Geographic.area[val] %in% over){
    full_labels[val] = "over"
  }
  else{
    full_labels[val] = "under"
  }
}
full_labels

full_GDP <- df_GDP[df_GDP$Country %in% intersection,"Value"]
full_NMR <- df_NMR[df_NMR$Geographic.area %in% intersection,c("Geographic.area","OBS_VALUE")]
full_ANC <- df_ANC[df_ANC$Geographic.area %in% intersection,"OBS_VALUE"]
full_IB  <- df_IB[df_IB$Geographic.area %in% intersection,"OBS_VALUE"]
full_WAT <- df_WAT[df_WAT$Geographic.area %in% intersection,"OBS_VALUE"]
full_SHP <- df_SHP[df_SHP$Country %in% intersection,"Births.attended.by.skilled.health.personnel...."]
full_SANI<- df_SANI[df_SANI$Geographic.area %in% intersection,"OBS_VALUE"]
full_LBW <- df_LBW[df_LBW$Geographic.area %in% intersection,"OBS_VALUE"]
full_data<- data.frame(full_NMR$Geographic.area, full_NMR$OBS_VALUE,full_ANC,full_IB,full_WAT,full_SANI,full_LBW,full_SHP, full_GDP)
colnames(full_data) <- data.frame("country", "NMR", "ANC", "IB", "WAT", "SANI", "LBW", "SHP", "GDP")


full_cluster<- kmeans(full_data[,-c(1,2)],2,nstart=100, iter.max = 1000)
table(full_cluster$cluster, full_labels)
adjustedRandIndex(full_labels,full_cluster$cluster)

x <- full_data[,-c(1,2)]
y <- as.factor(full_labels)

library(caret)

featurePlot(x=x,y=y,plot="ellipse")
?featurePlot
#Hypothesis Testing Round 2
t.test(full_total[full_total$full_labels == "over",]$ANC,full_total[full_total$full_labels == "under",]$ANC)
wilcox.test(full_total[full_total$full_labels == "over",]$ANC,full_total[full_total$full_labels == "under",]$ANC)

t.test(full_total[full_total$full_labels == "over",]$IB,full_total[full_total$full_labels == "under",]$IB)
wilcox.test(full_total[full_total$full_labels == "over",]$IB,full_total[full_total$full_labels == "under",]$IB)

t.test(full_total[full_total$full_labels == "over",]$WAT,full_total[full_total$full_labels == "under",]$WAT)
wilcox.test(full_total[full_total$full_labels == "over",]$WAT,full_total[full_total$full_labels == "under",]$WAT)

t.test(full_total[full_total$full_labels == "over",]$SANI,full_total[full_total$full_labels == "under",]$SANI)
wilcox.test(full_total[full_total$full_labels == "over",]$SANI,full_total[full_total$full_labels == "under",]$SANI)

t.test(full_total[full_total$full_labels == "over",]$LBW,full_total[full_total$full_labels == "under",]$LBW)
wilcox.test(full_total[full_total$full_labels == "over",]$LBW,full_total[full_total$full_labels == "under",]$LBW)

t.test(full_total[full_total$full_labels == "over",]$SHP,full_total[full_total$full_labels == "under",]$SHP)
wilcox.test(full_total[full_total$full_labels == "over",]$SHP,full_total[full_total$full_labels == "under",]$SHP)

t.test(full_total[full_total$full_labels == "over",]$GDP,full_total[full_total$full_labels == "under",]$GDP)
wilcox.test(full_total[full_total$full_labels == "over",]$GDP,full_total[full_total$full_labels == "under",]$GDP)
s <- Reduce(intersect,list(LBW$Geographic.area, SHP$Country, IB$Geographic.area, SANI$Geographic.area, WAT$Geographic.area))

s_NMR <- NMR[NMR$Geographic.area %in% s, "OBS_VALUE"]
s_WAT <- WAT[WAT$Geographic.area %in% s, "OBS_VALUE"]
s_SANI <- SANI[SANI$Geographic.area %in% s, "OBS_VALUE"]
s_LBW <- LBW[LBW$Geographic.area %in% s, "OBS_VALUE"]
s_IB <- IB[IB$Geographic.area %in% s, "OBS_VALUE"]
s_SHP <- SHP[SHP$Country %in% s, "Births.attended.by.skilled.health.personnel...."]


NMR_s <- NMR[NMR$Geographic.area %in% s,]
s_labels <- vector(mode="character", length=length(s))
for(val in 1:length(s_labels)){
  if(NMR_s$Geographic.area[val] %in% over){
    s_labels[val] = "over"
  }
  else{
    s_labels[val] = "under"
  }
}

s_data <- data.frame(NMR_s$Geographic.area, s_WAT, s_SANI, s_LBW, s_IB, s_SHP)
colnames(s_data) <- data.frame("country", "WAT", "SANI", "LBW", "IB", "SHP")

s_cluster<- kmeans(s_data[,-1],2,nstart=100, iter.max = 1000)
table(s_cluster$cluster, s_labels)


f_GDP2 <- GDP[GDP$Country.Name %in% s,]
df_GDP2 <- data.frame("Country"=f_GDP2$Country.Name, "Value" = f_GDP2$X2014)

GDP2_labels <- vector(mode="character", length=length(s))
for(val in 1:length(df_GDP2$Value)){
  if(df_GDP2$Value[val] < 1045){
    GDP2_labels[val] = "Low"
  }
  else if(df_GDP2$Value[val]>=1045 & df_GDP2$Value[val]<4125){
    GDP2_labels[val] = "Lower Mid"
  }
  else if(df_GDP2$Value[val]>=4125 & df_GDP2$Value[val]<12746){
    GDP2_labels[val] = "Upper Mid"
  }
  else if(df_GDP2$Value[val]>=12746){
    GDP2_labels[val] = "Upper"
  }
}

GDP2_labels

GDP_cluster<- kmeans(s_data[,-1],4,nstart=100, iter.max = 1000)
table(GDP_cluster$cluster, GDP2_labels)
library(factoextra)
# Dimension reduction using PCA
res.pca <- prcomp(full_data[, -c(1,2)],  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(full_cluster$cluster)
# Add Species groups from the original data sett
ind.coord$labels <- full_labels
# Data inspection

# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)
var <- get_pca_var(res.pca)

head(ind.coord)
library(ggpubr)
ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "labels", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)


fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)





######Testing Data

#Load In Testing Data:

unicef_2015 <- read.csv("2015_UNICEF.csv", header=TRUE)
ib_pt2 <- read.csv("2015_IB.csv", header=TRUE)
SHP_2015 <- read.csv("2015_SHP.csv", header = TRUE)
WAT_2015 <- unicef_2015[unicef_2015$Indicator=="Proportion of population using improved drinking water sources",]
SANI_2015 <- unicef_2015[unicef_2015$Indicator=="Proportion of population using improved sanitation facilities",]
LBW_2015 <- unicef_2015[unicef_2015$Indicator=="Prevalence of low birth weight among new-borns",]
ib_pt1 <- unicef_2015[unicef_2015$Indicator=="Institutional deliveries - percentage of deliveries in a health facility",]

unique(ib_pt1$Geographic.area)
unique(ib_pt2$Country)

dfib1 <- ib_pt1[,c(1,5)]
dfib2 <- ib_pt2[,c(1,3)]
colnames(dfib1) <- c("Country","Value")
colnames(dfib2) <- c("Country","Value")

unique_1 <- unique(dfib1$Country)
unique_2 <- unique(dfib2$Country)
unique.countries <- c(unique_1,unique_2)
unique.countries2 <- unique(unique.countries)
length(unique.countries2)

test.countries <- Reduce(intersect, list(unique.countries2, SHP_2015$Country, WAT_2015$Geographic.area, SANI_2015$Geographic.area, LBW_2015$Geographic.area))
length(test.countries) ##Find data that is in this

pre.SHP <- SHP_2015[SHP_2015$Country %in% test.countries,]
test.SHP <- SHP_2015[SHP_2015$Country %in% test.countries, "Births.attended.by.skilled.health.personnel...."]
pre.WAT <- WAT_2015[WAT_2015$Geographic.area %in% test.countries, "Geographic.area"]
test.WAT <- WAT_2015[WAT_2015$Geographic.area %in% test.countries, "OBS_VALUE"]
test.SANI <- SANI_2015[SANI_2015$Geographic.area %in% test.countries, "OBS_VALUE"]
test.LBW <- LBW_2015[LBW_2015$Geographic.area %in% test.countries, "OBS_VALUE"]

length(dfib1$Country)
length(unique_1)
dfib1
'%notin%' <- Negate('%in%')
ib.long <- dfib1[dfib1$Country %notin% dfib2$Country,]
ib.long2 <- rbind(ib.long, dfib2)
ib.long3 <- ib.long2[ib.long2$Country %in% test.countries,]
ib.long3
length(ib.long3$Country)
ib.long4 <- ib.long4[!duplicated(ib.long4$Country), ]
length(ib.long4$Country)

IB_2015 <- ib.long4[order(ib.long4$Country),]
test.IB <- IB_2015$Value

NMR_2015 <- read.csv("2015_NMR.csv", header=TRUE)

test.NMR <- NMR_2015[NMR_2015$Geographic.area %in% test.countries, "OBS_VALUE"]


test.labs <- vector(mode="character", length=length(test.countries))
for(val in 1:length(test.labs)){
  if(test.NMR[val] > 12){
    test.labs[val] = "over"
  }
  else{
    test.labs[val] = "under"
  }
}

test.data <- data.frame(test.countries,test.labs,test.WAT, test.SANI, test.LBW, test.IB, test.SHP)

test.data

train.data <- data.frame(s_labels,s_data)
train.data <- train.data[,c(2,1,3,4,5,6,7)]
train.data
require(mlbench)
library(e1071)
train.data$label <-s_labels
train.data$label
train.data$label <- ifelse((train.data$s_labels=="over"),1,0)
train.data$label <-as.factor(train.data$s_labels)
train.data$label

colnames(train.data) <- c("country","label","WAT","SANI","LBW","IB","SHP")
head(train.data)
colnames(test.data) <- c("country","label","WAT","SANI","LBW","IB","SHP")
head(test.data)

test.data$label <- ifelse((test.data$label=="over"),1,0)
test.data$label
sum(test.data$label)
test.data$s_labels <-as.factor(test.data$label)
head(train.data)
WHO.model<- naiveBayes(label ~ WAT+SANI+LBW+IB+SHP, data=train.data) 
predict(WHO.model, test.data[,-c(1,2)])
table(predict(WHO.model, test.data[,-c(1,2)]), test.data[,2])


library(e1071)

svm.model.who <- svm(label ~ WAT+SANI+LBW+IB+SHP, data=train.data, kernel = "radial", gamma = 0.5, cost = 10,cross=nrow(train.data))
summary(svm.model.who)
svm.pred.who <- predict(svm.model.who, test.data[,-c(1,2)])
cm <- table(predict(svm.model.who, test.data[,-c(1,2)]), test.data[,2])
cm
testing.accuracy <- (cm[1]+cm[4])/59


