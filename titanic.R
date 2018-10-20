#Reading the test and train datasets
train<-read.csv("D:/Titanic_infsci2725/train.csv", header=TRUE, sep=",")
test<-read.csv("D:/Titanic_infsci2725/test.csv", header=TRUE, sep=",")
#Displaying first few rows of train and test datasets
head(train)
head(test)
#examining the structure of the train and test datasets
str(train)
str(test)
#To get survival count 
table(train$Survived)

#To get survival proportion 
prop.table(table(train$Survived))

#Creating a new column in test dataset and initializing it to 0
test$Survived <- 0

#Displaying first few rows of test dataset after adding a new column
head(test)

#examining survival count based on gender 
table(train$Sex, train$Survived)

#examining survival proportion based on gender 
prop.table(table(train$Sex, train$Survived))

#Updating Survived column in testdataset
for(i in 1:nrow(test)) {
if(test$Sex[i] == 'female') {
  test$Survived[i] <-1
}
}
#Displaying first few rows
head(test)

