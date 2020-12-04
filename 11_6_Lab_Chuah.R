# 11/6 Lab

wine_data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep=",")

head(wine_data)

nrow(wine_data)
colnames(wine_data) <- c("Cvs", "Alcohol", 
                         "Malic_Acid", "Ash", "Alkalinity_of_Ash", 
                         "Magnesium", "Total_Phenols", "Flavanoids", "NonFlavanoid_Phenols",
                         "Proanthocyanins", "Color_Intensity", "Hue", "OD280/OD315_of_Diluted_Wine", 
                         "Proline")
head(wine_data) #now have column names


#Darker heatmaps = more correlated
?heatmap

heatmap(cor(wine_data),Rowv=NA,Colv=NA)

?factor
cultivar_classes<- factor(wine_data$Cvs)

?prcomp
wine_data_PCA <- prcomp(scale(wine_data[,-1]))
summary(wine_data_PCA)
#First 8 variables account for 92% of the variance