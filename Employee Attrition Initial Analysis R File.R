#Code to Clear Global Environment
rm(list=ls())                            
while (!is.null(dev.list()))  dev.off() 
getwd()

mydata<- read.csv("~/downloads/Employee Attrition data.csv")
View(mydata)
library(plotly)
library(lars)
library(glmnet)

#Summary of the data with Standard deviation and Number of observations
summary(mydata)
sd <- sapply(mydata, sd)
sd
length(mydata$satisfaction_level)

#Correlation Matrix to check correlation between the variables of employee data
library(ggcorrplot)
corr1 <- cor(mydata[c("satisfaction_level","last_evaluation_rating","projects_worked_on","average_montly_hours","time_spend_company","Attrition")])
corr1
ggcorrplot(corr1, title = "Correlation matrix of Employee data",hc.order=TRUE)

#Histogram and density Plot
hist(mydata$satisfaction_level,main="Frequency Histogram of Satisfaction level of employees",col="purple",xlab="Satisfaction level Indicator",ylab="Frequency")
plot(density(mydata$satisfaction_level),main="Density of Satisfaction level")
polygon(density(mydata$satisfaction_level),col="blue")

#Hypothesis testing
mysample<- subset(mydata,subset=(mydata$Attrition=="1"))
View(mysample)
length(mysample$Attrition)

t.test(mysample$satisfaction_level[200:1000], alternative = "less", mu = 6)
t.test(mysample$last_evaluation_rating[1200:2000], alternative = "less", mu = 5)

#Logistic regression Model
logitmodel<-glm(mydata$Attrition~mydata$satisfaction_level+mydata$last_evaluation_rating+mydata$projects_worked_on+mydata$average_montly_hours+mydata$time_spend_company+as.factor(mydata$Work_accident)+as.factor(mydata$promotion_last_5years)+as.factor(mydata$Department)+as.factor(mydata$salary))
summary(logitmodel)

# Use sample to divide data into trainig and testing
Sampledata <- sample(2, nrow(mydata),replace = TRUE, prob = c(0.7,0.3))
trainingSet <- mydata[Sampledata ==1,]
testingSet <- mydata[Sampledata ==2,]
dim(trainingSet)
dim(testingSet)
head(trainingSet)
head(testingSet)

#find lambdas
lambdas <- 10^seq(2, -3, by = -.1)

#lasso regression on training dataset
x1 <- cbind(trainingSet$satisfaction_level,trainingSet$last_evaluation_rating,trainingSet$projects_worked_on,trainingSet$average_montly_hours,trainingSet$time_spend_company,trainingSet$Work_accident,trainingSet$promotion_last_5years,trainingSet$salary)
y1 <- trainingSet$Attrition
model_lasso<-glmnet(x1,y1)
plot(model_lasso, xvar ="norm",label = TRUE)

cv_fit<-cv.glmnet(x=x1,y=y1,alpha = 1, nlamda = lambdas)
plot(cv_fit)

#best lambda
lambda_best <- cv_fit$lambda.min
lambda_best
cv_fit$lambda.1se

# fit model using lamba best
fit<-glmnet(x=x1,y=y1,alpha = 1,lambda = lambda_best)
fit$beta

#performacr analytics
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
a <- chart.Correlation(mydata, histogram = TRUE, pch=19)

# Prediction Testing Set
x2 <- cbind(testingSet$satisfaction_level,testingSet$last_evaluation_rating,testingSet$projects_worked_on,testingSet$average_montly_hours,testingSet$time_spend_company,testingSet$Work_accident,testingSet$promotion_last_5years,testingSet$salary)
y2 <- testingSet$Attrition

predictions_test <- predict(fit, s = lambda_best, newx = x2)
final <- cbind(y2, predictions_test)
head(final)
``
preds <- c(0.43, 0.20, 0.61, 0.18, 0.64,0.24)
actual <- c(1, 1, 1, 1, 1, 1)
rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss

#fit model using 1se lambda
fit1<-glmnet(x=x1,y=y1,alpha = 1, lambda = cv_fit$lambda.1se)
fit1$beta


#dtree_test <- rpart(Loan_Status ~ Credit_History+Education+Self_Employed+Property_Area+LogLoanAmount+
#                      LogIncome,method="class", data=testnew,parms=list(split="information"))
##DecisionTree 
install.packages("rpart")
library(rpart)

treeFit <- rpart(Attrition~., method="class", data=trainingSet)
print(treeFit)
#plot(treeFit)
rpart.plot(treeFit)

prediction <- predict(treeFit,newdata=testingSet, type = "class")
prediction

table_mat <- table(testingSet$Attrition, prediction)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
# 0-No , 1-Yes
# https://www.guru99.com/r-decision-trees.html
#https://www.geeksforgeeks.org/confusion-matrix-machine-learning/


#Fit Ridge model with lambda min
fit3<-glmnet(x=x1,y=y1,alpha = 0,lambda = cv_fit$lambda.min)
fit3$beta

#lasso regression on whole dataset
#x1 <- cbind(mydata$satisfaction_level,mydata$last_evaluation_rating,mydata$projects_worked_on,mydata$average_montly_hours,mydata$time_spend_company,mydata$Work_accident,mydata$promotion_last_5years,mydata$Department,mydata$salary)
#y1 <- mydata$Attrition
#model_lasso<-glmnet(x1,y1)
#plot(model_lasso, xvar ="norm",label = TRUE)

