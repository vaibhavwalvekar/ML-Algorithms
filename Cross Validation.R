library(ISLR)
attach(Auto)
set.seed(1)

#Performing validation set approach
l = rep(0,10)
for (i in 1:10)
{
  train = sample(392,196)
for (j in 1:10){
  lm.fit=lm(mpg~poly(horsepower,j),data=Auto,subset=train)
  l[j] = mean((mpg-predict(lm.fit,Auto))[-train]^2)
    }
  par(new=T)
  plot(l, ylim=c(16.0,26.0), ylab = "Mean Squared Error", xlab = "Degree of Polynomial")
  lines(l)
}


##Leave one out Cross Validation
library(boot)
cv.error=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
par(mfrow=c(1,1))
plot(cv.error, ylim=c(16.0,26.0), ylab = "Mean Squared Error", xlab = "Degree of Polynomial")
lines(cv.error)


##10 fold Cross Validation

cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
par(new=T)
plot(cv.error.10, ylim=c(16.0,26.0), ylab = "Mean Squared Error", xlab = "Degree of Polynomial")
lines(cv.error.10)
  
## Fitting least squares linear modelto prdict mpg using polynomials of degrees 1 to 10
l = rep(0,10)
  for (j in 1:10){
    lm.fit=lm(mpg~poly(horsepower,j),data=Auto)
    l[j] = mean((mpg-predict(lm.fit,Auto))^2)
  }
  par(new=T)
  plot(l, ylim=c(16.0,26.0), ylab = "Mean Squared Error", xlab = "Degree of Polynomial")
  lines(l)
  
  ## Fitting least squares linear modelto prdict mpg using polynomials of degrees 10
lm.fit2 = lm(mpg~poly(horsepower,10),data=Auto)
summary(lm.fit2)  
