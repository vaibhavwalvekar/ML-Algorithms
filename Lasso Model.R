library(ISLR)
library(glmnet)

## Generating random predictor with n=30

set.seed(10)
x = rnorm(30, mean = 0, sd = 1)
noise = rnorm(30, mean = 0, sd = 1)

## Generating a response accordint to below model
y = 3-2*x+3*(x^2)+noise

## Fitting a Lasso model
grid=10^seq(10,-2,length=100)
xpoly=poly(x,7)
##xbind = cbind(x,x^2,x^3,x^4,x^5,x^6,x^7)
lasso.mod=glmnet(x=xpoly,y,alpha=1, lambda=grid)

plot(lasso.mod,xvar="lambda")
legend('topright', c("1","2","3","4","5","6","7") ,  bty='n', lty=1,col=c('black', 'red', 'green',' blue', 'light blue', 'pink','black'), cex=.75, lwd=c(2.5,2.5))


## Using Cross validation to select tuning parameter
cv.lasso.mod <- cv.glmnet(xpoly,y=y,alpha=1)
plot(cv.lasso.mod)
min_lambda <- cv.lasso.mod$lambda.min
min_lambda

## Fitting Lasso model
lasso.mod1=glmnet(xpoly,y,alpha=1, lambda =min_lambda )
coef(lasso.mod1)


## Fitting test data
x1= rnorm(1000)
noise1=rnorm(1000)

y1= 3- 2*x1 + 3*(x1^2) + noise1
x1poly=poly(x1,7)
#lasso.mod1=glmnet(poly(x1,7),y=y1,alpha=1)

lasso.pred1=predict (lasso.mod1,s=min_lambda,newx=x1poly)
mean(( lasso.pred1 -y1)^2)
