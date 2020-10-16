### Lab 2 ###

##Lab 2a##

EPI_data <- read.csv("2010EPI_data.csv", header = TRUE, skip = 1)
head(EPI_data)
summary(EPI_data)

#Measures of Central Tendency#

EPI <- EPI_data$EPI
DALY <- EPI_data$DALY

summary(EPI) #all modes of central tendency for the dataset
med_EPI <- fivenum(EPI, na.rm = TRUE) #returns minimum, lower quartile, median, upper quartile, and maximum
med_EPI
avg_EPI <- mean(EPI) #the average
avg_EPI
summary(DALY)
med_DALY <- fivenum(DALY, na.rm = TRUE)
med_DALY
avg_DALY <- mean(DALY)
avg_DALY


#Histogram and distribution plots#
hist(EPI)
hist(DALY)

ENVHEALTH <- EPI_data$ENVHEALTH
ECOSYSTEM <- EPI_data$ECOSYSTEM

boxplot(ENVHEALTH, ECOSYSTEM)

qqplot(ENVHEALTH, ECOSYSTEM)

#Linear and Least Squares#
EPI_data_2 <- read.csv("2010EPI_data.csv", header = TRUE, skip = 1)
attach(EPI_data_2)

boxplot(ENVHEALTH, DALY, AIR_H, WATER_H)
#looking at the regression for envhealth specifically, i.e. just 1 variable
lmENVH <- lm(ENVHEALTH ~ DALY+AIR_H+WATER_H)

lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)

#Predict#


DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))
NEW <- data.frame(DALYNEW, AIR_HNEW, WATER_HNEW)

pENV <- predict(lmENVH, NEW, interval = "prediction")

cENV <- predict(lmENVH, NEW, interval = "confidence")

#Repeat for Air_E

boxplot(AIR_E, DALY, AIR_H, WATER_H)

lmAIR <- lm(AIR_E ~ DALY, AIR_H, WATER_H)
cAIR <- coef(lmAIR)
NEW_AIR <- data.frame(DALYNEW, AIR_HNEW, WATER_HNEW)

pAIR <- predict(lmAIR, NEW_AIR, interval = "prediction")

coAIR <- predict(lmAIR, NEW, interval = "confidence")

#Repeat for Climate


boxplot(CLIMATE, DALY, AIR_H, WATER_H)

lmCLI <- lm(CLIMATE ~ DALY, AIR_H, WATER_H)
cCLI <- coef(lmAIR)
NEW_CLI <- data.frame(DALYNEW, AIR_HNEW, WATER_HNEW)

pCLI <- predict(lmAIR, NEW_AIR, interval = "prediction")

coCLI <- predict(lmAIR, NEW, interval = "confidence")


#Ecosystem with AIR_E and Climate

lmECO <- lm(ECOSYSTEM ~ AIR_E+CLIMATE)
cECO <- coef(lmECO)
AIR_ENEW <- c(seq(5,95,5))
CLIMATENEW <- c(seq(5,95,5))
NEW_ECO <- data.frame(AIR_ENEW, CLIMATENEW)

pECO <- predict(lmECO, NEW_ECO, interval = "prediction")

# We want the most relevant variable for one region

# Sort the dataset based on region first
