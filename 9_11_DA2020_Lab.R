
days <- c('Mon','Tue', 'Wed','Thur','Fri','Sat','Sun')
temp <- c(28, 30.5, 32, 31.2, 29.3, 27.9, 26.4)
snowed <- c('T','T','F','F','T','T','F')


RPI_Weather_Week <- data.frame(days, temp, snowed)

head(RPI_Weather_Week) #shows first 6 rows, data frame concatenates vectors as columns

str(RPI_Weather_Week) #shows the structure, basically 3 columns of length 7 pasted together side by side

summary(RPI_Weather_Week)

RPI_Weather_Week[1,] #data for first row, all columns
RPI_Weather_Week[,1] #data for first column, all rows

#Column names are the names of the vectors used to make the dataframe

RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']

RPI_Weather_Week[1:5, c("days","temp")] #Data for first 5 entries in dataframe columns wth names "days" and "temp", i.e., temperature for first five days

subset(RPI_Weather_Week, subset=snowed==TRUE) #All entries for which snowed is True

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed #sorted in terms of this column, but sorts all data

RPI_Weather_Week[sorted.snowed]




################ EPI Dataset ######################

data1 <- read.csv("2010EPI_data.csv", header = TRUE, skip = 1)
names(data1)

attach(data1)
fix(data1)

### Exercise 1 (Look at different statistical aspecs of the EPI column data, see if we can expand on powerpoint functions to find different aspects of the data, main focus here is identification)

summary(data1$EPI) #specifically looking at EPI data, haven't done any analysis/vis yet
fivenum(data1$EPI) #does fivenum tell us anything about our dataset really?  Find different statistics about our data to see what is relevant (simple statistics, nothing complex like classification, ML, etc., still in summary statistics)


help(stem)
stem(data1$EPI)
hist(data1$EPI)
hist(data1$EPI, seq(30, 95, 1), prob=TRUE)
lines(density(data1$EPI, na.rm = TRUE, bw = 1.))

rug = (data1$EPI)

epiquantile <- quantile(data1$EPI)


plot(ecdf(data1$EPI), do.points = FALSE, verticals = TRUE)
par(pty="s")
qqnorm(data1$EPI)


x <- seq(30,95,1)
qqplot(qt(ppoints(250, df = 5, x, xlab="Q-Q plot for tdsn")))
qqline(x)


#PRACTICE
names(data1)
ehealth <- data1$ENVHEALTH
daly <- data1$DALY

summary(ehealth)
summary(daly)
fivenum(ehealth)
fivenum(daly)
plot(ecdf(ehealth), do.points=FALSE, verticals=TRUE)


#Looking at Environmental health and DALY data, look at fitting a density function to the histogram data
hist(ehealth, prob= TRUE, col='blue')
lines(density(ehealth), col = 'red', lwd = 2)
lines(density(ehealth, adjust =2), lty="dotted", col="pink", lwd=2)

hist(daly, prob = TRUE, col='blue')
lines(density(daly), col = 'red', lwd = 2)
lines(density(daly, adjust =2), lty="dotted", col="pink", lwd=2)


boxplot(data1$EPI, ehealth, daly)
qqplot(data1$EPI, ehealth)

qqplot(data1$EPI, daly)


#Just assign groups we are comparing to local variables
epi <- data1$EPI
envhealth <- ehealth
ecosystem <- data1$ECOSYSTEM
air_h <- data1$AIR_H
water_h <- data1$WATER_H
air_e <- data1$AIR_E
water_e <- data1$WATER_E
biodiversity <- data1$BIODIVERSITY



#Comparing data
comparisons = data.frame(epi, envhealth, ecosystem, air_h, water_h, air_e, water_e, biodiversity) #(Just for practice with using data.frame)
summary(comparisons)
boxplot(epi, envhealth, ecosystem, air_h, water_h, air_e, water_e, biodiversity)



EPILand <- epi[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland,x,prob=TRUE)

#Inspect the data to make sure operations are applicable to data, i.e. don't want to find quantiles on a vector of strings

GRUMP_data <- read.csv("GPW3_GRUMP_SummaryInformation_2010.csv", header = TRUE)

summary(GRUMP_data)

population <- GRUMP_data$Sum.All.Urban.Extents..pop.
area <- GRUMP_data$Area

summary(population)
fivenum(population)
boxplot(population)
hist(population)
hist(area)
