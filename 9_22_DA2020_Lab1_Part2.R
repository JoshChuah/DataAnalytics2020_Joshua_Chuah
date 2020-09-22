###Lab 1 Part 2###
library(ggplot2)
EPI_data <- read.csv("2010EPI_data.csv", header = TRUE, skip = 1)
summary(EPI_data)


##Import a bunch of quantities we want to look at
epi <- EPI_data$EPI
envhealth <- EPI_data$ENVHEALTH
ecosystem <- EPI_data$ECOSYSTEM
air_h <- EPI_data$AIR_H
water_h <- EPI_data$WATER_H
air_e <- EPI_data$AIR_E
water_e <- EPI_data$WATER_E
biodiversity <- EPI_data$BIODIVERSITY

##Lets say we want to see how air pollution and water pollution are distributed in populations

##We may want to see how envhealth correlates with air and water pollution in a population

##Air_h and Water_h are the effects of air pollution and water pollution on humans
em <- lm(envhealth ~ air_h+water_h)
cem <- coef(em)
summary(em)$coef

plot(envhealth~ air_h+water_h)
abline(em, col = 2)

plot(envhealth, air_h)

plot(envhealth, water_h)

### Look at how air_h and water_h are distributed

plot(ecdf(air_h), do.points=FALSE, verticals = TRUE)
plot(ecdf(water_h), do.points = TRUE, verticals = TRUE)




help(qqnorm)
par(pty='s')
qqnorm(air_h)
qqline(air_h)

qqnorm(water_h)
qqline(water_h)


dnorm(air_h)
x1 <- air_h
y1 <- dnorm(air_h, mean = mean(air_h), sd = sd(air_h))
plot(x1,y1)

dnorm(water_h)
x2 <- water_h
y2 <- dnorm(air_h, mean = mean(water_h), sd = sd(water_h))
plot(x2,y2)

### Fit the probability density function of the data to a histogram of the data values

hist(air_h, prob= TRUE, col='blue')
lines(density(air_h), col = 'red', lwd = 2)

hist(water_h, prob= TRUE, col='blue')
lines(density(water_h), col = 'red', lwd = 2)

###Now lets try to do the same thing using ggplot2
help(ggplot)
help(geom_histogram)
help(stat_function)

a1 <- ggplot(EPI_data$air_h, aes(x = air_h))+geom_histogram(binwidth = 4)+stat_function(fun=dnorm, color = 'red', args = list(mean = mean(air_h, na.rm = TRUE), sd = sd(air_h, na.rm=TRUE)))
a1

w1 <- ggplot(EPI_data$water_h, aes(x = water_h))+geom_histogram(binwidth = 4)+stat_function(fun=dnorm, color = 'red', args = list(mean = mean(water_h, na.rm = TRUE), sd = sd(water_h, na.rm=TRUE)))
w1
