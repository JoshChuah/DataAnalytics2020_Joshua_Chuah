#https://aquarius.tw.rpi.edu/html/DA/

multivariate <- read.csv("multivariate.csv", header = TRUE)

attach(multivariate)
names(multivariate)
multivariate

plot(Income, Immigrant, main = "Scatterplot")
plot(Immigrant, Homeowners)


help(lm)
mm <- lm(Homeowners ~ Immigrant)
mm
plot(Immigrant, Homeowners)

abline(mm)
abline(mm, col=2, lwd = 3)

summary(mm)
attributes(mm)
mm$coefficients

cm <- coef(mm)


HP <- multivariate$Homeowners/multivariate$Population
PD <- multivariate$Population/multivariate$area
mm2 <- lm(multivariate$Immigrant ~ multivariate$Income+Population+HP+PD)

cm2 <- coef(mm2)

abline(mm2)