# Anthony Val C. Camposano
# June, 1, 2015
# This is the first exercise to understand randomForest, guided by the blog of Trevor Stephens



test <- read.csv("test.csv")
train <- read.csv("train.csv")

#a quick look at the structure of the dataframe
str(test)
str(train)

#By default, R will import text strings as factors
train <- read.csv("train.csv",stringsAsFactors=FALSE)
test <- read.csv("test.csv",stringsAsFactors=FALSE)


#accessing the columns of a dataframe, use dollar sign operator 
columnSurvived = train$Survived

#one the basic summary statistics functions in R
table(columnSurvived)

#If one wants the proportion of the table, use another function
prop.table(table(train$Survived))


# Perhaps it is good to assume that everyone in the test set died. 
# Let's add our 'everyone dies' prediction to the test set dataframe
# This command will create the column 'SURVIVED' in the dataframe

test$Survived <- rep(0,418)

# Practice creating a csv file 
submit<-data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish_1.csv")



