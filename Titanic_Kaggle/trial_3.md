# Anthony Val C. Camposano
# June, 1, 2015
# Automating the process using decision trees, guided by the blog of Trevor Stephens

test <- read.csv("test.csv")
train <- read.csv("train.csv")

# we're going to use machine learning to build decision trees to do the heavy lifting 
# for us

# they are the basis for some of the powerful and popular machine learning algorithms

# Recursive partitioning and Regression Trees - rpart
# CART decision tree algorithm 

library(rpart)

# the format of rpart is similar to aggregate, you feed an equation, followed
# by the variable of interest and variables used for prediction

fit <- rpart(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train,method="class")
plot(fit)
text(fit)

# for a better visualization of the decision tree we have to install some libraries
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)

# to make prediction from this tree doesn't require all the subsetting and overwriting we did 
# last lesson. 

Prediction <- predict(fit,test,type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit,file="predictionbasedondecisiontree.csv",row.names=FALSE)


# we'll introduce control in the parameters of the decision tree growing
# what if we override the default by getting more and more complex in the decision tree
fit <- rpart(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=train,method="class",control=rpart.control(minsplit=2,cp=0))
fancyRpartPlot(fit)

Prediction <-predict(fit,test,type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit,file="overfittingdecisiontree.csv",row.names=FALSE)

# you can play with various control parameters we saw in rpart.control help file


