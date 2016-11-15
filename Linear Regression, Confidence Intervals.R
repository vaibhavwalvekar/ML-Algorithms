library(MASS)
library(ISLR)

##Running linear regression on the said predictor and response.
slr <- lm(mpg~horsepower, data=Auto)

## Deriving summary of the linear regression
summary(slr)

## Predicting mpg associated with horsepower = 98
predict.lm(slr, data.frame(horsepower=98))

## Predicting 95% confidence intervals
predict.lm(slr, data.frame(horsepower=98),interval = "confidence")

## Predicting 95% prediction intervals
predict.lm(slr, data.frame(horsepower=98),interval = "prediction")

## Plotting scatter plot of mpg vs horsepower and drawing abline
plot(Auto$horsepower, Auto$mpg, main = "mpg vs horsepower", xlab = "horsepower", ylab = "mpg", col = "blue")
abline(slr, col = "black")

## diagnostic plots of the least squares regression fit
layout(matrix(c(1,2,3,4),2,2))
plot(slr)