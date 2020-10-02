#9_15_Coursework

library(ggplot2)
data("diamonds")
?diamonds
summary(diamonds)

hist(diamonds$carat)

ggplot(data = diamonds) + geom_bar(mapping = aes(x=cut))
ggplot(data = diamonds) + geom_histogram(mapping = aes(x=carat), binwidth = 0.1)

#length will probably be similar to width, whereas depth will be different (thinking about stereotypical diamond)
x <- diamonds$x
y <- diamonds$y
z <- diamonds$z

boxplot(x,y,z)


ggplot(data = diamonds) + geom_histogram(mapping = aes(x = x), binwidth = 0.5)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = y), binwidth = 0.5)
ggplot(data = diamonds) + geom_histogram(mapping = aes(x = z), binwidth = 0.5)
#Also explore different binwidths for different patterns, too high might make data look like it is all one value, too low may make the data look flat


##Explore Distribution of price
