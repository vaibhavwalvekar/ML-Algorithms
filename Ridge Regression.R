library(leaps)
library(ISLR)
library(glmnet)
library (MASS)
set.seed (10)

## Generating random data for ridge regression
x= rnorm(30)
noise=rnorm(30)
y= 3- 2*x + 3*(x^2) + noise
xpoly=poly(x,7)

x1= rnorm(1000)
noise1=rnorm(1000)
y1= 3- 2*x1 + 3*(x1^2) + noise1
x1poly=poly(x1,7)

grid =10^ seq (10,-2, length =100)

## Plotiign value of each coefficient  as a function of lambda
ridge.mod=glmnet(x=xpoly,y=y,alpha=0, lambda = grid)
plot(ridge.mod,xvar="lambda", label = TRUE)
legend('topright', c("1","2","3","4","5","6","7") ,  bty='n', lty=1,col=c('black', 'red', 'green',' blue', 'light blue', 'pink','black'), cex=.75, lwd=c(2.5,2.5))


##Using cross validation to select tuning parameter lambda
cv.ridgemod <- cv.glmnet(xpoly,y=y,alpha=0)
plot(cv.ridgemod)
min_lambda <- cv.ridgemod$lambda.min
min_lambda

## Fitting Ridge regression
ridge.pred=predict (ridge.mod ,s=min_lambda,newx=xpoly)
mean(( ridge.pred -y)^2)

out=glmnet (xpoly,y,alpha =0,lambda = min_lambda)
ridge.coef=predict (out ,type ="coefficients",s=min_lambda )
ridge.coef

##Fittign ridge regression on test data
ridge.pred1=predict(out,s=min_lambda,newx=x1poly)
mean((ridge.pred1 -y1)^2)


##Fitting least squares model and comparing with ridge regrssion
set.seed(1)
xl1<-rnorm(30)
noise1<-rnorm(30)
yl1<-3-(2*xl1)+(3*(xl1^2))+noise1
xlpoly<-poly(xl1,7)
ls.mod<-lm(yl1~xlpoly)

xl2<-rnorm(1000)
noise2<-rnorm(1000)
xl2poly<-poly(xl2,7)
yl2<-3-(2*xl2)+(3*(xl2^2))+noise2

ls.pred<-predict(ls.mod,newx=xl2poly)
mean((ls.pred-yl2)^2)
