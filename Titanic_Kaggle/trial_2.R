# Anthony Val Camposano
# Part 2: The Gender-Class Model
# Improving our prediction capability

test <- read.csv('test.csv')
train <- read.csv('train.csv')


# Disaster has a bias to save the "women and the children first"
# Well looked at the gender of the passengers.
summary(train$Sex)

# The proportion table command, a two-way comparison on the number of males and females 
# Proportion table command takes each entry in the table and divides by the total # of passengers
prop.table(table(train$Sex,train$Survived))


# We want to see the row-wise proportions as separate groups. 
# 1 is for row-wise proportion, 2 is for column-wise proportions
prop.table(table(train$Sex,train$Survived),1)


#We're going to update our prediction
test$Survived <- 0 
test$Survived[test$Sex == 'female'] <- 1


#==========================================================
#           2nd Submittion - improved accuracy by 14%
#==========================================================
#Submitting our second prediction with a little improvement. 
submit1 <- data.frame(PassengerId = test$PassengerId,Survived = test$Survived)
write.csv(submit1,file = 'allwomensurvive.csv',row.names = FALSE)

# Examining the age variable
# Some data values are missing in data analytics this can cause a variety of problems
summary(train$Age)

# we have a continuous variable which makes table useless, we'll create a new variable
# to indicate whether the passenger is below the age of 18:
train$Child <- 0 
train$Child[train$Age < 18] <- 1

# We want to create a table for both gender and age to see the survival proportions 
# for different subsets. Proportion table isn't equipped for this, we instead use aggregate
aggregate(Survived~Child+Sex, data=train,FUN = length)
aggregate(Survived~Child+Sex, data=train,FUN = sum)

aggregate(Survived~Child + Sex, data=train,FUN=function(x){sum(x)/length(x)})

# It still appears that if a passenger is female most survive, and if they were male most dont,
# regardless of whether they were a child or not. Nothing to change in our predictions

# Its interesting to look at the class of the passenger and what they paid for their ticket
# The Fare is a continuous variable so again we are going to discretize the values for easy tabulation

train$Fare2<-'30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20]<-'20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10]<-'10-20'
train$Fare2[train$Fare < 10] <-'10'

# Lets run a longer aggregate to see something interesting here
aggregate(Survived~Fare2+Pclass+Sex, data=train,FUN=function(x){sum(x)/length(x)})

# Most of the class 3 women who paid more than 20 dollar also miss out on a lifeboat,
test$Survived <- 0 
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0


#===============================================================
#           3nd Submittion -- improved our accuracy by 1.5%
#===============================================================
#Submitting our second prediction with a little improvement. 
submit <- data.frame(PassengerId = test$PassengerId,Survived = test$Survived)
write.csv(submit,file = 'allwomensurvivewithfarelessthan20.csv',row.names = FALSE)







