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
barplot(table(train$Survived), names.arg= c("Perished", "Survived"), col= "Black")

#Creating a new column in test dataset and initializing it to 0
test$Survived <- 0

#Write submission file
submission1 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submission1, file = "Prediction1.csv", row.names = FALSE)

#Displaying first few rows of test dataset after adding a new column
head(test)

#examining survival count based on gender 
table(train$Sex, train$Survived)

#examining survival proportion based on gender 
prop.table(table(train$Sex, train$Survived))

prop.table(table(train$Sex, train$Survived),1)

# using '1', we group by the first dimension which is train$sex
prop.table(table(train$Sex, train$Survived),1)

# visualization
train$Sex<-as.factor(train$Sex)
train$Survived<-as.factor(train$Survived)
plot(train$Sex, train$Survived)


#Updating Survived column in test dataset
for(i in 1:nrow(test)) {
if(test$Sex[i] == 'female') {
  test$Survived[i] <-1
}
}


#Write submission file
submission2 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submission2, file = "Prediction2.csv", row.names = FALSE)

#Displaying first few rows
head(test)

# Replacing missing age with Average age
train[is.na(train[,'Age']), 'Age'] <- mean(train[,'Age'], na.rm = TRUE)

#Creating a new column in test dataset
train$Child[train$Age < 18] <- "Child"
train$Child[train$Age > 18] <- "Adult"

#examining survival proportion based on age(Child) 
prop.table(table(train$Child, train$Survived))
prop.table(table(train$Child, train$Survived), 1)

# visualization
train$Child<-as.factor(train$Child)
train$Survived<-as.factor(train$Survived)
plot(train$Child, train$Survived)


test$Survived[test$Age < 18]<-1
submission3 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submission3, file = "Prediction3.csv", row.names = FALSE)

summary(train$Fare)
prop.table(table(train$Fare<14, train$Sex, train$Survived))

aggregate(train$Survived, by= list(train$Sex, train$Fare<15), FUN=sum)
# test$Survived[test$Fare > 15 & test$Sex == 'female']<-1
# submission4 <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
# write.csv(submission4, file = "Prediction4.csv", row.names = FALSE)

prop.table(table(train$Pclass, train$Sex, train$Survived))

install.packages('party')
library(party)
# png(file = "titanic_decision_tree.png")
titanic_decision_tree <- ctree(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train)
plot(titanic_decision_tree)
#dev.off()


# Classification Tree with rpart
install.packages('rpart')
library(rpart)

# grow tree 
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, method="class",  data = train)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, main="Classification Tree for titanic")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "tree.ps", title = "Classification Tree for titanic")

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)


Prediction <- predict(fit, test, type = "class")


submission4 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submission4, file = "Prediction4.csv", row.names = FALSE)
