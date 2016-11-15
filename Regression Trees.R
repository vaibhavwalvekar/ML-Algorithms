library(ISLR)
library(tree)
library(MASS)
attach(Auto)
set.seed(10)
tree.auto=tree(mpg~.-name,Auto,control=tree.control(392, mincut = 1, mindev = 0))
summary(tree.auto)
plot(tree.auto)
text(tree.auto,pretty=0)
tree.auto

train=sample(1:nrow(Auto), 300)
Auto.test=Auto[-train,]
mpg.test=mpg[-train]
tree.auto=tree(mpg~.-name,control=tree.control(300, mincut = 2, minsize = 4, mindev = 0.0001),Auto,subset=train)

summary (tree.auto)
plot(tree.auto)
text(tree.auto, pretty = 0)

cv.auto=cv.tree(tree.auto)
names(cv.auto)
cv.auto
par(mfrow=c(1,2))
plot(cv.auto$size,cv.auto$dev,type="b")
plot(cv.auto$k,cv.auto$dev,type="b")
tree.min = which.min(cv.auto$dev)
points(cv.auto$size[tree.min], cv.auto$dev[tree.min], col = "green", cex = 2, pch = 20)

cv.auto$dev

prune.auto=prune.tree(tree.auto,best=11)
plot(prune.auto)
text(prune.auto,pretty=0)

yhat=predict(prune.auto,newdata=Auto.test,type="vector")
plot(yhat,mpg.test)
abline(0,1)
mean((yhat-mpg.test)^2)