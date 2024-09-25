library(readxl) #read excel files
#library(tidyverse)
#library(ROSE) #balance imbalanced data
library(glmnet)
library(caret) # test/training data splitting, fitting regression and classification models
#library(e1071)
#library(PRROC) #roc curve
#library(doParallel)
#library(Rcpp)
#library(MASS)

# read excel file
Raw_credit <- as.data.frame(read_excel("credit.xls", skip = 1))
credit<-Raw_credit

#Renaming columns
colnames(credit)[7] <- "PAY_1"
colnames(credit)[25] <- "default"
#removing the customer index
credit <- credit[,-1]
#fixing errors in the data - similar to week 2
credit$MARRIAGE[credit$MARRIAGE == 0] <- 3
credit$EDUCATION[credit$EDUCATION %in% c(0,5,6)] <- 4
#Changing categorical vars to factor - leaving the PAY variables as they cause issues as factors.
credit[,c(2:4,24)] <- lapply(credit[,c(2:4,24)], FUN=factor) # factor: SEX, EDUCATION, MARRIAGE, default


set.seed(1)
index <- sample(1:length(credit$LIMIT_BAL), 0.7*length(credit$LIMIT_BAL)) # "replace = FALSE"
#index <- createDataPartition(credit$default, p = 0.7, list = FALSE) #alt method
mean(credit$default==1) # can take mean of booleans.
x_train <- credit[index, -24] # all independent variables in the training set
y_train <- credit[index, 24] # the dependent variable "default" in the training set
Data_train<-credit[index,] # X and Y, all of training
x_test <- credit[-index, -24] # all independent variables in the test set
y_test <- credit[-index, 24] # the dependent variables in the test set
Data_test<-credit[-index,] # X and Y of testing

#balancing
downsampled <- downSample(x_train, y_train, yname="default")
 # when you do this, I think it reblanaces your y. We discovered that 'default' was 
 # imbalanced so this is an option.
x_train2 <- downsampled[,-24]
y_train2 <- downsampled[,24]
Data_train2 <- downsampled


#---- modellling ----
# first just transform testing x to a matrix, for future function calls.
x_test.m <- as.matrix(x_test)
for (i in c(1,5:23)) {
  x_test.m[,i] <- as.numeric(x_test.m[,i])
}
#logistic

logistic <- glm(formula = default ~ . , data=downsampled, family=binomial)
logistic$coefficients
logistic.pred.logit <- predict(object=logistic, newx=x_test.m)
logistic.pred <- 1 / (1 + exp(-logistic.pred.logit)); head(logistic.pred)
mean(logistic.pred)
# getting penalty
x_train2.m <- as.matrix(x_train2)
for (i in c(1,5:23)) {
  x_train2.m[,i] <- as.numeric(x_train2.m[,i])
}

a <- cv.glmnet(x=x_train2.m, y=y_train2, family="binomial")
lambda <- a$lambda.min #min or 1se?

# alpha is 1 for lasso, 0 for ridge
# ridge
ridge <- glmnet(x=x_train2.m, y=y_train2, family="binomial", alpha=0, lambda=lambda)
coef(ridge)
ridge.pred.logit <- predict(ridge, newx=x_test.m)
ridge.pred <- 1 / (1 + exp(-ridge.pred.logit)); head(ridge.pred)
mean(ridge.pred)
mean(as.numeric(y_test)-1)
mean((ridge.pred - (as.numeric(y_test)-1))^2)

lasso <- glmnet(x=x_train2.m, y=y_train2, family="binomial", alpha=1, lambda=lambda)
coef(lasso)
lasso.pred.logit <- predict(object=lasso, newx=x_test.m)
lasso.pred <- 1 / (1 + exp(-lasso.pred.logit)); head(lasso.pred)
mean(lasso.pred)
mean(as.numeric(y_test)-1)
mean((lasso.pred - (as.numeric(y_test)-1))^2)

elast <- glmnet(x=x_train2.m, y=y_train2, family="binomial", alpha=0.5, lambda=lambda)
coef(elast)
elast.pred.logit <- predict(object=elast, newx=x_test.m)
elast.pred <- 1 / (1 + exp(-elast.pred.logit)); head(elast.pred)
mean(elast.pred)
mean(as.numeric(y_test)-1)
mean((elast.pred - (as.numeric(y_test)-1))^2)

#all predictions are super similar.

# ----unbalanced modelling----
# getting penalty
x_train.m <- as.matrix(x_train)
a <- cv.glmnet(x=x_train.m, y=y_train, family="binomial")
lambda <- a$lambda.min #min or 1se?

ridge <- glmnet(x=x_train.m, y=y_train, family="binomial", alpha=0, lambda=lambda)
coef(ridge)
ridge.pred.logit <- predict(ridge, newx=x_test.m)
ridge.pred <- 1 / (1 + exp(-ridge.pred.logit)); head(ridge.pred)
mean(ridge.pred)
mean(as.numeric(y_test)-1)
mean((ridge.pred - (as.numeric(y_test)-1))^2)

lasso <- glmnet(x=x_train.m, y=y_train, family="binomial", alpha=1, lambda=lambda)
coef(lasso)
lasso.pred.logit <- predict(object=lasso, newx=x_test.m)
lasso.pred <- 1 / (1 + exp(-lasso.pred.logit)); head(lasso.pred)
mean(lasso.pred)
mean(as.numeric(y_test)-1)
mean((lasso.pred - (as.numeric(y_test)-1))^2)

elast <- glmnet(x=x_train.m, y=y_train, family="binomial", alpha=0.5, lambda=lambda)
coef(elast)
elast.pred.logit <- predict(object=elast, newx=x_test.m)
elast.pred <- 1 / (1 + exp(-elast.pred.logit)); head(elast.pred)
mean(elast.pred)
mean(as.numeric(y_test)-1)
mean((elast.pred - (as.numeric(y_test)-1))^2)

# all around 0.145 innaccruacy by MSE.
