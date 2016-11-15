library(ISLR)

attach(Auto)
set.seed(1)

## Creating new variable HighMPG as below
med = median(mpg)
HighMPG = rep (0,nrow(Auto ))
HighMPG [mpg >med]=1
data.frame(Auto ,HighMPG)

##Plotting horsepower vs. displacement
plot(horsepower, displacement, col=ifelse(HighMPG==1,"blue","red"), main = "Horsepower vs. Displacement")
legend("bottomright", c("mpg above median", "mpg below median"), col = c("blue", "red"), pch = c(1, 1))

## Applying validation set approach to estimate test error
rowcount = nrow(Auto)
train <- sample(rowcount, as.integer(0.50*rowcount))
trainingset<-Auto[train,]  ## 50% rows of auto dataset
testset <- Auto[-train,]  ## 50% rows of auto dataset
testHighMPG <- HighMPG[-train]

train.X <- cbind(trainingset$displacement,trainingset$horsepower )
test.X <- cbind(testset$displacement, testset$horsepower )
train.HighMPG <- HighMPG[train]
library(class)
l = rep(0,100)
for (i in 1:100){
  knn.pred = knn(train.X, test.X, as.factor(train.HighMPG), k=i)
  l[i] = mean(knn.pred!=testHighMPG)
plot(l, ylab = "Estimated Test Error", xlab = "K values for KNN" , main = "Estimated test error for predicting HighMPG using Horsepower and Displacement")
print(l)
}

## Performing k nearest neighbors regression
rowcount = nrow(Auto)
train <- sample(rowcount, as.integer(1.0*rowcount))
trainingset<-Auto[train,]  ## 50% rows of auto dataset
train.X <- cbind(trainingset$cylinders, trainingset$displacement, trainingset$horsepower, trainingset$weight, trainingset$acceleration, trainingset$year,trainingset$origin,trainingset$mpg )
train.HighMPG <- HighMPG[train]
library(class)
l = rep(0,100)
for (i in 1:100){
  knn.pred = knn(train.X, train.X, as.factor(train.HighMPG), k=i)
  l[i] = mean(knn.pred!=train.HighMPG)}
plot(l, type = "l", ylab = "Estimated Train Error", xlab = "K values for KNN" , main = "Estimated train error for predicting HighMPG")

