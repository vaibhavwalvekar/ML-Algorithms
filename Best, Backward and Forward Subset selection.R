library(leaps)
library(ISLR)

## Generating predictor of length n = 100

set.seed(1)
x = rnorm(100, mean = 0, sd = 1)
noise = rnorm(100, mean = 0, sd = 1)

## Generating a response variable according to below model
y = 3-2*x+(x^2)+noise

## Performing Best subset selection
df = data.frame(x,x^2,x^3,x^4,x^5,x^6,x^7,y)
attach(df)
reg.fit = regsubsets(y~., data=df)
reg.sum=summary(reg.fit)

par(mfrow=c(2,2))
plot(reg.sum$rss,xlab="Number of Variables",ylab="RSS", type='l')
plot(reg.sum$adjr2,xlab="Number of Variables",ylab="Adjusted R^2", type='l')
points(which.max(reg.sum$adjr2), reg.sum$adjr2[which.max(reg.sum$adjr2)], col="blue",cex=2,pch=8)
plot(reg.sum$cp, xlab="Number of Variables",ylab="Cp",type='l')
points(which.min(reg.sum$cp), reg.sum$cp[which.min(reg.sum$cp)], col="blue",cex=2,pch=8)
plot(reg.sum$bic,xlab="Number of Variables",ylab="BIC", type='l')
points(which.min(reg.sum$bic), reg.sum$bic[which.min(reg.sum$bic)], col="blue",cex=2,pch=8)
coef(reg.fit,7)

## Performing Forward subset selection
regfwd.fit=regsubsets(y~., data=df, nvmax=20, method="forward")
regfwd.sum=summary(regfwd.fit)

par(mfrow=c(2,2))
plot(regfwd.sum$rss,xlab="Number of Variables",ylab="RSS", type='l')
plot(regfwd.sum$adjr2,xlab="Number of Variables",ylab="Adjusted R^2", type='l')
points(which.max(regfwd.sum$adjr2), regfwd.sum$adjr2[which.max(regfwd.sum$adjr2)], col="blue",cex=2,pch=8)
plot(regfwd.sum$cp, xlab="Number of Variables",ylab="Cp",type='l')
points(which.min(regfwd.sum$cp), regfwd.sum$cp[which.min(regfwd.sum$cp)], col="blue",cex=2,pch=8)
plot(regfwd.sum$bic,xlab="Number of Variables",ylab="BIC", type='l')
points(which.min(regfwd.sum$bic), regfwd.sum$bic[which.min(regfwd.sum$bic)], col="blue",cex=2,pch=8)
coef(regfwd.fit,7)

## Performing Backward subset selection
regbwd.fit=regsubsets(y~., data=df,nvmax=20, method="backward")
regbwd.sum=summary(regbwd.fit)

par(mfrow=c(2,2))
plot(regbwd.sum$rss,xlab="Number of Variables",ylab="RSS", type='l')
plot(regbwd.sum$adjr2,xlab="Number of Variables",ylab="Adjusted R^2", type='l')
points(which.max(regbwd.sum$adjr2), regbwd.sum$adjr2[which.max(regbwd.sum$adjr2)], col="blue",cex=2,pch=8)
plot(regbwd.sum$cp, xlab="Number of Variables",ylab="Cp",type='l')
points(which.min(regbwd.sum$cp), regbwd.sum$cp[which.min(regbwd.sum$cp)], col="blue",cex=2,pch=8)
plot(regbwd.sum$bic,xlab="Number of Variables",ylab="BIC", type='l')
points(which.min(regbwd.sum$bic), regbwd.sum$bic[which.min(regbwd.sum$bic)], col="blue",cex=2,pch=8)
coef(regbwd.fit,7)
