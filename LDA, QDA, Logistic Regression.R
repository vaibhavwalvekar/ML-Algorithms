library(MASS)
library(ISLR)
library(class)
## Assigning Auto data to auto variable
auto = Auto
attach(auto)

## Calculating median value of mpg column from auto dataset
medmpg = median(mpg)

## creating binary variable mpg01, which takes value as 1 for above mpg else 0. Using data.frame to add it to auto
mpg01 = rep (0,nrow(auto ))
mpg01 [mpg >medmpg]=1
mpg01 =as.factor (mpg01)
auto =data.frame(auto ,mpg01)

## finding relations between mpg01 and other variables in auto dataset
pairs(auto)
par(mfrow=c(2,2))
boxplot(weight~mpg01, main="mpg01 vs. weight")
boxplot(cylinders~mpg01, main="mpg01 vs. cylinders")
boxplot(displacement~mpg01, main="mpg01 vs. displacement")
boxplot(horsepower~mpg01, main="mpg01 vs. horsepower")

## splitting data into training set and test set
set.seed(1)
rowcount = nrow(auto)
train <- sample(rowcount, as.integer(0.66*rowcount))
trainingset<-auto[train,]  ## 66% rows of auto dataset
testset <- auto[-train,]  ## 34% rows of auto dataset
testmpg01 <- mpg01[-train]

## doing lda on training set and using it on test set
train.lda <- lda(mpg01 ~ cylinders + weight + displacement +horsepower, data = trainingset)
test.lda <- predict(train.lda, testset)
table(test.lda$class, testmpg01)
mean(test.lda$class != testmpg01)

## doing qda on trainingset and using it on test set
train.qda <- qda(mpg01 ~ cylinders + weight + displacement +horsepower, data = trainingset)
test.qda <- predict(train.qda, testset)
table(test.qda$class, testmpg01)
mean(test.qda$class != testmpg01)

## doing logistic regression on trainingset and using it on test set
train.glm <- glm(mpg01 ~ cylinders + weight + displacement +horsepower, data = trainingset, family = binomial)
summary(train.glm)
probability <- predict(train.glm, testset, type = "response")
print(probability)
pred.glm <- rep(0, length(probability))
pred.glm[probability > 0.5] <- 1
table(pred.glm, testmpg01)
mean(pred.glm != testmpg01)

## doing KNN analysis on training and test dataset

train.X <- cbind(trainingset$cylinders, trainingset$weight, trainingset$displacement,trainingset$horsepower )
test.X <- cbind(testset$cylinders, testset$weight, testset$displacement, testset$horsepower )
train.mpg01 <- trainingset$mpg01

set.seed(1)
knn.pred = knn(train.X, test.X, as.factor(train.mpg01), k=1)
table(knn.pred, testmpg01)
mean(knn.pred!=testmpg01)

knn.pred = knn(train.X, test.X, as.factor(train.mpg01), k=5)
table(knn.pred, testmpg01)
mean(knn.pred!=testmpg01)

knn.pred = knn(train.X, test.X, as.factor(train.mpg01), k=10)
table(knn.pred, testmpg01)
mean(knn.pred!=testmpg01)

knn.pred = knn(train.X, test.X, as.factor(train.mpg01), k=50)
table(knn.pred, testmpg01)
mean(knn.pred!=testmpg01)

knn.pred = knn(train.X, test.X, as.factor(train.mpg01), k=100)
table(knn.pred, testmpg01)
mean(knn.pred!=testmpg01)

knn.pred = knn(train.X, test.X, as.factor(train.mpg01), k=200)
table(knn.pred, testmpg01)
mean(knn.pred!=testmpg01)
