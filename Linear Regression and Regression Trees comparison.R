library(ISLR)
library(MASS)
library(tree)

## Generating a linear model
x1= rnorm(100,mean = 0, sd = 1)
x2= rnorm(100,mean = 0, sd = 1)
noise = rnorm(100,mean = 0, sd = 1)
y= 1+ 2*x1+ 3*x2 + noise

##Plotting data
plot(x1, x2, col=rainbow(200)[rank(y)], pch=15)

##Plotting linear model
plot(y,x1+x2)

##Fitting linear model and regression tree and comparing them 
ls.mod<-lm(y~x1+x2)
summary(ls.mod)

tree.fit = tree(y~x1+x2,control=tree.control(100, mincut = 1, mindev = 0))
summary(tree.fit)
plot(tree.fit)
text(tree.fit,pretty=0)

cv.fit =cv.tree(tree.fit)
plot(cv.fit$size ,cv.fit$dev ,type='b')
tree.min = which.min(cv.fit$dev)
points(cv.fit$size[tree.min], cv.fit$dev[tree.min], col = "blue", cex = 2, pch = 20)

prune.fit=prune.tree(tree.fit, best=10)
summary(prune.fit)
plot(prune.fit)
text(prune.fit, pretty = 0)


##Partitioned feature space
prune.fit=prune.tree(tree.fit, best=5)
summary(prune.fit)
plot(prune.fit)
text(prune.fit, pretty = 0)
plot(x1, x2, col=rainbow(200)[rank(y)], pch=20)
partition.tree(prune.fit, ordvars=c("x1","x2"), add=TRUE)


## Testing out models
set.seed(10)
x1=rnorm(1000)
x2=rnorm(1000)
noise=rnorm(1000)
y1=1+2*x1+3*x2+noise
newdf=data.frame(x1,x2)

yhat=predict (ls.mod,newdata =newdf)
mean((yhat-y1)^2)

yhat1=predict (tree.fit,newdata =newdf)
mean((yhat1-y1)^2)
