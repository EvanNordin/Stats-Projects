require(tree)
require(randomForest)
require(gbm)
firstYear <- read.csv("FirstYearGPA.csv")


#classification trees

#dividing dataset into training and test set
set.seed(98)
train <- sample(1:nrow(firstYear),nrow(firstYear)/2)
test <- firstYear[-train,]
#Creating a tree for the dataset
tree.gpa <- tree(GPA~.,firstYear,subset=train)
summary(tree.gpa)
plot(tree.gpa)
text(tree.gpa,pretty = 0)

#Pruning our tree using CV
cv.gpa <- cv.tree(tree.gpa, K=5)
cv.gpa
plot(cv.gpa$size,cv.gpa$dev,type='b')

#We found that the best number of terminal nodes is somewhere between 4 and 8
prune.gpa <- prune.tree(tree.gpa,best=4)
summary(prune.gpa)
plot(prune.gpa)
text(prune.gpa, pretty=0)

#Predictions on test set, we will do predictions with pruned and unpruned for comparing
yhat <- predict(tree.gpa,newdata=test)
plotTest<- firstYear$GPA[-train]
plot(yhat,plotTest)
abline(0,1)

#Test MSE
mean((yhat-plotTest)^2)         #MSE = 0.241
sqrt(mean((yhat-plotTest)^2))   #Root MSE = 0.491

#Now checking pruned model
yhat <- predict(prune.gpa,newdata=test)
plotTest<- firstYear$GPA[-train]
plot(yhat,plotTest, xlab="Predicted Value",ylab="Actual Value")
abline(0,1)

mean((yhat-plotTest)^2)         #MSE = 0.182
sqrt(mean((yhat-plotTest)^2))   #Root MSE = 0.427

#Bagging and Random Forests
set.seed(32)

#To perform bagging we set m = # of predictors
bag.gpa <- randomForest(GPA~.,data=firstYear,subset = train,mtry=9, importance=TRUE)
bag.gpa

yhat.bag<- predict(bag.gpa,newdata=test)
plot(yhat.bag,plotTest,xlab="Predicted Value", ylab= "Actual Value" , main ="Making Predictions with Bagging Model")
abline(0,1)
mean((yhat.bag-plotTest)^2)       #MSE = 0.142
sqrt(mean((yhat.bag-plotTest)^2)) #Root MSE = 0.377

#rf with m = 3
rf1.gpa <- randomForest(GPA~.,data = firstYear,subset = train,mtry=3,importance=TRUE)
rf1.gpa

yhat.rf<- predict(rf1.gpa,newdata=test)
plot(yhat.rf,plotTest,xlab="Predicted Value", ylab= "Actual Value" , main ="Making Predictions with RF M = 3")
abline(0,1)
mean((yhat.rf-plotTest)^2)       #MSE = 0.148
sqrt(mean((yhat.rf-plotTest)^2)) #Root MSE = 0.385

#rf with m = 5
rf2.gpa <- randomForest(GPA~.,data = firstYear,subset = train,mtry=5,importance=TRUE)
rf2.gpa

yhat.rf<- predict(rf2.gpa,newdata=test)
plot(yhat.rf,plotTest,xlab="Predicted Value", ylab= "Actual Value" , main ="Making Predictions with RF M = 5")
abline(0,1)
mean((yhat.rf-plotTest)^2)       #MSE = 0.141
sqrt(mean((yhat.rf-plotTest)^2)) #Root MSE = 0.376

#Checking out variable importance
importance(bag.gpa)
importance(rf1.gpa)
importance(rf2.gpa)
#Boosting

boost.gpa <- gbm(GPA~.,data=firstYear[train,],distribution="gaussian",n.trees=5000)
summary(boost.gpa)
plot(boost.gpa,i="HSGPA")
