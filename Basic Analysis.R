library(MASS)
library(ISLR)

## Generating scatterplot matrix for all variables in Auto dataset 
pairs(Auto)

## checking all variable names in Auto dataset and removing 'name' variable for correlations.  
names(Auto)
cor(Auto[1:8])

## running lm function over all variables excluding name and storing it in mlr variable
mlr <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year +origin , data = Auto)
summary(mlr)

## using 3 interaction and checking their significance on response by using summary function
mlri <- lm(mpg ~ cylinders * horsepower+year * weight, data = Auto[, 1:8])
summary(mlri)

## Using displacement, weight, acceleration and horsepower variables for comparison
par(mfrow = c(2, 2))
plot(log(Auto$displacement), Auto$mpg)
plot(sqrt(Auto$displacement), Auto$mpg)
plot((Auto$displacement)^2, Auto$mpg)

par(mfrow = c(2, 2))
plot(log(Auto$weight), Auto$mpg)
plot(sqrt(Auto$weight), Auto$mpg)
plot((Auto$weight)^2, Auto$mpg)

par(mfrow = c(2, 2))
plot(log(Auto$acceleration), Auto$mpg)
plot(sqrt(Auto$acceleration), Auto$mpg)
plot((Auto$acceleration)^2, Auto$mpg)

par(mfrow = c(2, 2))
plot(log(Auto$horsepower), Auto$mpg)
plot(sqrt(Auto$horsepower), Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)

