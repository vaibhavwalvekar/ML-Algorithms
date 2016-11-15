library(MASS)
library(ISLR)
library(class)
library(tree)
library(gbm)
library(randomForest)

attach(Carseats)

medsales = median(Sales)

salesnew = rep ("Bad",nrow(Carseats ))
salesnew [Sales >medsales]="Good"
salesnew =as.factor (salesnew)
carseats =data.frame(Carseats ,salesnew)

set.seed(1)
train <- sample(nrow(carseats), nrow(carseats) / 2)
carseats$salesnew <- ifelse(carseats$salesnew == "Good", 1, 0)
carseats.train <- carseats[train, ]
carseats.test <- carseats[-train, ]


logit.fit <- glm(salesnew ~ Advertising + Age+Education , data = carseats.train, family = "binomial")
logit.probs <- predict(logit.fit, newdata = carseats.test, type = "response")
logit.pred <- ifelse(logit.probs > 0.5, 1, 0)
table(carseats.test$salesnew, logit.pred)

boost.fit <- gbm(salesnew ~ Advertising + Age+Education , data = carseats.train, distribution = "bernoulli", n.trees = 5000)
boost.probs <- predict(boost.fit, newdata = carseats.test, n.trees = 5000)
boost.pred <- ifelse(boost.probs > 0.5, 1, 0)
table(carseats.test$salesnew, boost.pred)

bag.fit <- randomForest(salesnew ~ Advertising + Age+Education , data = carseats.train, mtry = 2)

bag.probs <- predict(bag.fit, newdata = carseats.test)
bag.pred <- ifelse(bag.probs > 0.5, 1, 0)
table(carseats.test$salesnew, bag.pred)

rf.fit <- randomForest(salesnew ~ Advertising + Age+Education, data = carseats.train)

rf.probs <- predict(rf.fit, newdata = carseats.test)
rf.pred <- ifelse(rf.probs > 0.5, 1, 0)
table(carseats.test$salesnew, rf.pred)


dfpred = data.frame(logistic=logit.pred, boosting = boost.pred, bagging=bag.pred, randomforest=rf.pred)

y = carseats.test$salesnew
sapply(dfpred, function(dfpred) mean((dfpred - y)^2))
