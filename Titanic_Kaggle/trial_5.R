# Anthony Val C. Camposano
# June, 1, 2015
# Random  Forest, guided by the blog of Trevor Stephens

# setting the directory
setwd("~/Analysis with Programming/R-programming/Titanic_Kaggle")
test <- read.csv("test.csv")
train <- read.csv("train.csv")

# Since the formulas for building a single decision tree are the same everytime, some
# source of randomness must be introduce to make trees different from one another, 
# this can be achieved by using 2 proccess.
# 1. bagging, for bootstrap aggregating - takes a randomized sample of the rows in your training set with replacement
sample(1:10, replace=TRUE)


# 2. Take only a subset of the records, typically the square root of the number available

# Through this sources of randomness, the ensemble contains a collection of totally unique trees which all make their classification differently.
# each tree is called to make a classification for a given passenger, the votes are tallied (with perhaps many hundreds, or thousands of trees) and the majority decision is chosen.

# Note: randomForest requires that the dataset does not contain any missing values
# rpart's great advantage uses surrogate variables when it encounters an NA value

# Let's grow tree on the subset of the data with the age values available
Agefit <- rpart(Age~Pclass + Sex + SibSp +Parch + Fare + Embarked + Title+FamilySize,data=combi[!is.na(combi$Age),],method="anova")
combi$Age[is.na(combi$Age)]<-predict(Agefit, combi[is.na(combi$Age),])

# embarked has a blank for 2 passengers
which(combi$Embarked == '')
combi$Embarked[c(62,830)] = 'S'

#other naughty variable - those with a missing value
combi$Fare[which(is.na(combi$Fare))]<-median(combi$Fare,na.rm=TRUE)

# Another restriction of randomForest: it can only digest factors up to 32 levels only
# FamilyID has 78 levels which exceeded our restricion 
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

# We're good to go, we're goint to test and setand grow a random forest
library(randomForest)
set.seed(415)

# we now have our engineered variables
train <- combi[1:891,]
test <- combi[892:1309,]

# randomForest algorithm
fit <- randomForest(as.factor(Survived)~Pclass +Sex + Age +SibSp +Parch +Fare+ Embarked +Title+FamilySize+FamilyID2, data=train,importance=TRUE,ntree=2000)

# Looking at what variables were important
varImpPlot(fit)


# Prediction for randomForest
Prediction <- predict(fit,test)
submit <-data.frame(PassengerID = test$PassengerId, Survived = Prediction)
write.csv(submit, file="firstforest.csv",row.names=TRUE)


# lets try a forest of conditional inference trees
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived)~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, controls=cforest_unbiased(ntree=2000,mtry=3))



# Prediction for conditional inference tress 
Prediction<-predict(fit,test,OOB=TRUE,type="response")
submit <-data.frame(PassengerID = test$PassengerId, Survived = Prediction)
write.csv(submit, file="conditionalforest.csv",row.names=FALSE)



