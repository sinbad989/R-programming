# Anthony Val C. Camposano
# June, 1, 2015
# Feature engineering, guided by the blog of Trevor Stephens

test = read.csv("test.csv")
train = read.csv("train.csv")

# library updates
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# a model with great feature can outperform a complicated algorithm with poor ones.
# Feature engineering - "easily the most important factor" in determining the success
# or failure of your predictive model

# It boils down to the human element in machine learning
# How much do you understand the data, with your human intuition and creativity, can make the difference

# In general, an engineering feature are more easier for a machine learning to digest

test$Survived <- NA
combi <- rbind(train,test)

#Names are still encoded as factors
combi$Name <- as.character(combi$Name)
strsplit(combi$Name[1],split='[,.]')[[1]]
strsplit(combi$Name[1],split='[,.]')[[1]][2]

#R has a function that apply more complication function one row at a time.
combi$Title <- sapply(combi$Name,FUN=function(x){strsplit(x,split='[,.]')[[1]][2]})

# we want to strip off those spaces from the beginning of each titles.
combi$Title<-sub(' ','',combi$Title)

# we want to combine some weird titles
combi$Title[combi$Title %in% c('Mlle','Mme')] <- 'Mlle'
combi$Title[combi$Title %in% c('Captain','Don','Major','Sir')]<-'Sir'
combi$Title[combi$Title %in% c('Dona','Jonkheer','Lady','the Countess')]<-'Lady'

# we then return the titles into being a Factor
combi$Title <- factor(combi$Title)

# Parch and SibSb combination as a family
combi$FamilySize <- combi$Parch + combi$SibSp + 1 

# concerns with the Surname
combi$Surname <-sapply(combi$Name,FUN=function(x){strsplit(x,split='[,.]')[[1]][1]})
combi$FamilyID <-paste(as.character(combi$FamilySize),combi$Surname,sep="")

# we can knock out the family of size less than 2
combi$FamilyID[combi$FamilSize <= 2]<-'small'
famID <- data.frame(table(combi$FamilyID))

# we want to subset this familyID after seeing that some family doesnt follow our assumption
famID <- famID[famID$Freq <=2,]

# we need to overwrite any family IDs in our dataset for groups that were not correctly identified
combi$FamilyID[combi$FamilyID %in% famID$Var1] <- 'small'
combi$FamilyID <- factor(combi$FamilyID)

# we now have our engineered variables
train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,data=train,method="class")
fancyRpartPlot(fit)

#Prediction

Prediction <- predict(fit,test,type='class')
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit,file="featureengineeringdecisiontree.csv",row.names=FALSE)


