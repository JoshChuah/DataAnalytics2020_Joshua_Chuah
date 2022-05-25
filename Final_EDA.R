###Final EDA



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
f_GDP <- GDP[GDP$Country.Name %in% sub_countries,]
df_GDP <- data.frame("Country"=f_GDP$Country.Name, "Value" = f_GDP$X2014)
df_GDP <- df_GDP[order(df_GDP$Country),]
df_GDP


#######
##Hypothesis Testing
#######
library(ggplot2)
over <- NMR[NMR$OBS_VALUE > 12,]$Geographic.area
under <- NMR[NMR$OBS_VALUE <= 12,]$Geographic.area

over_ANC <- ANC[ANC$Geographic.area %in% over,]$OBS_VALUE
under_ANC <- ANC[ANC$Geographic.area %in% under,]$OBS_VALUE
wilcox.test(over_ANC, under_ANC)
t.test(over_ANC, under_ANC)
var.test(over_ANC, under_ANC)

a <- data.frame(group = "high NMR", value = over_ANC)
b <- data.frame(group = "low NMR", value = under_ANC)

plot.data <- rbind(a,b)
ggplot(plot.data, aes(x=group, y=value, fill=group)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=10, color="red", fill="red")


over_IB <- IB[IB$Geographic.area %in% over,]$OBS_VALUE
under_IB <- IB[IB$Geographic.area %in% under,]$OBS_VALUE
wilcox.test(over_IB, under_IB)
t.test(over_IB, under_IB)
var.test(over_IB, under_IB)

a <- data.frame(group = "high NMR", value = over_IB)
b <- data.frame(group = "low NMR", value = under_IB)

plot.data <- rbind(a,b)
ggplot(plot.data, aes(x=group, y=value, fill=group)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=10, color="red", fill="red")

over_WAT <- WAT[WAT$Geographic.area %in% over,]$OBS_VALUE
under_WAT <- WAT[WAT$Geographic.area %in% under,]$OBS_VALUE
wilcox.test(over_WAT, under_WAT)
t.test(over_WAT, under_WAT)
var.test(over_WAT, under_WAT)

a <- data.frame(group = "high NMR", value = over_WAT)
b <- data.frame(group = "low NMR", value = under_WAT)

plot.data <- rbind(a,b)
ggplot(plot.data, aes(x=group, y=value, fill=group)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=10, color="red", fill="red")

over_SANI <- SANI[SANI$Geographic.area %in% over,]$OBS_VALUE
under_SANI <- SANI[SANI$Geographic.area %in% under,]$OBS_VALUE
wilcox.test(over_SANI, under_SANI)
t.test(over_SANI, under_SANI)
var.test(over_SANI, under_SANI)

a <- data.frame(group = "high NMR", value = over_SANI)
b <- data.frame(group = "low NMR", value = under_SANI)

plot.data <- rbind(a,b)
ggplot(plot.data, aes(x=group, y=value, fill=group)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=10, color="red", fill="red")

over_LBW <- LBW[LBW$Geographic.area %in% over,]$OBS_VALUE
under_LBW <- LBW[LBW$Geographic.area %in% under,]$OBS_VALUE
wilcox.test(over_LBW, under_LBW)
t.test(over_LBW, under_LBW)
var.test(over_LBW, under_LBW)

a <- data.frame(group = "high NMR", value = over_LBW)
b <- data.frame(group = "low NMR", value = under_LBW)

plot.data <- rbind(a,b)
ggplot(plot.data, aes(x=group, y=value, fill=group)) + geom_boxplot() + stat_summary(fun=mean, geom="point", shape=20, size=10, color="red", fill="red")



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






















library(party)
NMR_tree <- ctree(NMR ~ ANC+IB+WAT+SANI+LBW+SHP+GDP, data=full_data)
plot(NMR_tree)
over <- NMR[NMR$OBS_VALUE > 12,]$Geographic.area
under <- NMR[NMR$OBS_VALUE <= 12,]$Geographic.area


sub_countries<- Reduce(intersect, list(ANC$Geographic.area, IB$Geographic.area, WAT$Geographic.area, SANI$Geographic.area))

target_countries <- sub_countries[sub_countries %in% over]
target_countries

df_NMR <- NMR[NMR$Geographic.area %in% sub_countries,]
df_ANC <- ANC[ANC$Geographic.area %in% sub_countries,]
df_ANC <- df_ANC[df_ANC$TIME_PERIOD == "2014",]
df_IB <- IB[IB$Geographic.area %in% sub_countries,]
df_IB <- df_IB[df_IB$TIME_PERIOD == "2014",]
df_WAT <- WAT[WAT$Geographic.area %in% sub_countries,]
df_SANI <- SANI[SANI$Geographic.area %in% sub_countries,]
df_SHP <- SHP[SHP$Country %in% sub_countries,]
df_LBW <- LBW[LBW$Geographic.area %in% sub_countries,]

five_var <- df_SHP$Country
five_NMR <- df_NMR[df_NMR$Geographic.area %in% five_var,]
five_ANC <- df_ANC[df_ANC$Geographic.area %in% five_var,]
five_WAT <- df_WAT[df_WAT$Geographic.area %in% five_var,]
five_IB <- df_IB[df_IB$Geographic.area %in% five_var,]
five_SANI <- df_SANI[df_SANI$Geographic.area %in% five_var,]

###Hypothesis testing

ANC_over <- df_ANC[df_ANC$Geographic.area %in% target_countries,"OBS_VALUE"]
ANC_under <- df_ANC[!df_ANC$Geographic.area %in% target_countries,"OBS_VALUE" ]
boxplot(ANC_over,ANC_under)
t.test(ANC_over,ANC_under)
wilcox.test(ANC_over,ANC_under)

high_contrib <- data.frame(full_NMR$Geographic.area,full_IB,full_WAT,full_SANI,full_LBW,full_SHP)

full2_cluster<- kmeans(high_contrib[,-1],2,nstart=100, iter.max = 1000)
table(full2_cluster$cluster, full_labels)


expanded_data <- data.frame(five_var, df_SHP$Births.attended.by.skilled.health.personnel....,five_ANC$OBS_VALUE, five_IB$OBS_VALUE, five_SANI$OBS_VALUE, five_WAT$OBS_VALUE)
colnames(expanded_data) <- c("Country", "SHP","ANC", "IB", "SANI", "WAT")
head(expanded_data)
total_dataset <- data.frame(sub_countries, df_ANC$OBS_VALUE, df_IB$OBS_VALUE, df_WAT$OBS_VALUE, df_SANI$OBS_VALUE, df_NMR$OBS_VALUE)
colnames(total_dataset) <- data.frame("Country", "ANC", "IB", "WAT", "SANI", "NMR")


f_labels <- vector(mode="character", length=length(sub_countries))

for(val in 1:length(f_labels)){
  if(df_NMR$Geographic.area[val] %in% target_countries){
    f_labels[val] = "over"
  }
  else{
    f_labels[val] = "under"
  }
}

f_labels


#f2_labels <- vector(mode="character", length=length(five_NMR$Geographic.area))

#for(val in 1:length(f2_labels)){
 # if(five_NMR$Geographic.area[val] %in% target_countries){
  #  f2_labels[val] = "over"
  #}
  #else{
   # f2_labels[val] = "under"
  #}
#}

f_labels



h_labels <- vector(mode="character", length=length(sub_countries))


for(val in 1:length(f_labels)){
  if(df_NMR$OBS_VALUE[val] > 20){
    h_labels[val] = "highest"
  }
  else if(df_NMR$OBS_VALUE[val] <= 20 & df_NMR$OBS_VALUE[val] > 12){
    h_labels[val] = "high"
  }
  else if(df_NMR$OBS_VALUE[val] <= 12 & df_NMR$OBS_VALUE[val] > 6){
    h_labels[val] = "low"
  }
  else if(df_NMR$OBS_VALUE[val] <= 6){
    h_labels[val] = "lowest"
  }
}
h_labels



GDP_labels <- vector(mode="character", length=length(sub_countries))
for(val in 1:length(df_GDP$Value)){
  if(df_GDP$Value[val] < 1045){
    GDP_labels[val] = "Low"
  }
  else if(df_GDP$Value[val]>=1045 & df_GDP$Value[val]<4125){
    GDP_labels[val] = "Lower Mid"
  }
  else if(df_GDP$Value[val]>=4125 & df_GDP$Value[val]<12746){
    GDP_labels[val] = "Upper Mid"
  }
  else if(df_GDP$Value[val]>=12746){
    GDP_labels[val] = "Upper"
  }
}

GDP_labels

g_cluster<- kmeans(total_dataset[,-1],4,nstart=1000, iter.max = 1000)
table(g_cluster$cluster, GDP_labels)

f_cluster<- kmeans(total_dataset[,-1],2,nstart=100, iter.max = 1000)
table(f_cluster$cluster, f_labels)

f_hcluster <- hclust()

highres_cluster<- kmeans(total_dataset[,-1],4,nstart=100, iter.max = 1000)
table(highres_cluster$cluster, h_labels)

f2_cluster <- kmeans(expanded_data[,-1],2,nstart=100, iter.max = 1000)
table(f2_cluster$cluster, f2_labels)

e_class <- ifelse(f2_labels %in% c("over"), 1, 2)
rand.index(e_class,f2_cluster$cluster)

library(mclust)
adjustedRandIndex(f2_cluster$cluster, f2_labels)
adjustedRandIndex(g_cluster$cluster, GDP_labels)

true_labels <- ifelse(f_labels %in% c("over"), 1, 2)
rand.index(true_labels,f_cluster$cluster)

true_labels2 <- as.numeric(as.factor(GDP_labels))
rand.index(true_labels2, g_cluster$cluster)
library(party)
fit <- ctree(NMR ~ WAT+SANI+IB+ANC, data = total_dataset)
plot(fit)

##Hierarchical Agglomerative

d <- dist(total_dataset[,-1], method = "euclidean")
fit <- hclust(d, method = "ward")
par(mar = rep(2,4))
plot(fit)
groups <- cutree(fit, k = 2)
rect.hclust(fit, k =2, border="red")

library(factoextra)

fviz_cluster(g_cluster, data = total_dataset[, -1],
             palette = c("#2E9FDF", "#00AFBB","#FC4E07","#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



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

vhead(ind.coord)
library(ggpubr)
ggscatter(
  ind.coord, x = "Dim.1", y = "Dim.2", 
  color = "cluster", palette = "npg", ellipse = TRUE, ellipse.type = "convex",
  shape = "labels", size = 1.5,  legend = "right", ggtheme = theme_bw(),
  xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
  ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
  stat_mean(aes(color = cluster), size = 4)

df_GDP$Value
