library(glmnet)
Data <- read.csv("insurance.csv")
#Divide data into X and Y
Data_X <- Data[,-7]
Data_Y <- Data[,7]
X_matrix <- model.matrix(~., Data_X) #change dataX factor variables into dummies
Y_matrix <- as.matrix(Data_Y)

#Linear model

model_linear<- lm(charges~., data=Data)
#Lasso-linear model
set.seed(2023)
CV_lasso<-cv.glmnet(X_matrix, Y_matrix, family="gaussian",
 alpha = 1, nfolds = 10)
model_lasso <- glmnet(X_matrix, Y_matrix, lambda = CV_lasso$lambda.min, alpha = 1)

#Generalized linear model with log link
model_loglinear<- glm(charges~., data=Data, family = gaussian(link="log") )

#----Testing----
# AIC and BIC, validation set approach, cross-validation approach,
#and bootstrap approach

# can replace Log(L) with Sigma Hat.
# Sigma Hat is found with RSS/n

insample <- function(model, testX) {
  y_hat <- predict(model, testX) # use Data X for lm, use Dummy variable Matrix form for glms.
  rss <- t(y_hat- Data_Y) %*% (y_hat - Data_Y)
  variance <- var(y_hat) # unrelated...
  df <- ncol(testX) - 1 + 1 # 9-1 betas, +1 variance.
  AIC <- nrow(testX) * log(rss) + 2 * df 
  BIC <- nrow(testX) * log(rss) + log(nrow(testX)) * df 
  return(matrix(data=c("AIC", "BIC", "RSS", AIC, BIC, rss), nrow=2, ncol=3, byrow=TRUE))
}

insample(model_linear, Data_X)
insample(model_lasso, X_matrix)
insample(model_loglinear, Data_X)

#----Testing validation set (VS) ----
train_indices <- sample(nrow(X_matrix), 0.7*nrow(X_matrix))
train_X <- X_matrix[train_indices,]
train_Y <- Y_matrix[train_indices,]
test_X <- X_matrix[-train_indices,]
test_Y <- Y_matrix[-train_indices,]

model_linear_VS <- lm(charges ~ . , data= Data[train_indices,] )
CV_lasso_VS<-cv.glmnet(train_X, train_Y, family="gaussian",
                    alpha = 1, nfolds = 10) # use same training data?
model_lasso_VS <- glmnet(train_X, train_Y, lambda = CV_lasso_VS$lambda.min, alpha=1)
model_loglinear_VS <- glm(charges~. , data=Data[train_indices, ], family=gaussian(link="log"))

VS_test <- function(model, testX, testY) {
  y_hat <- predict(model, testX)
  rss <- t(y_hat- testY) %*% (y_hat - testY)
  return(rss/nrow(testX))
}
VS_test(model_linear_VS, Data_X[-train_indices,], Data_Y[-train_indices])
VS_test(model_lasso_VS, test_X, test_Y)
VS_test(model_loglinear_VS, Data_X[-train_indices,], Data_Y[-train_indices]) #extremely large

#---- CV ----
#Randomly shuffle the data
CVData<-Data[sample(nrow(Data)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(CVData)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
cverr_lin <- c()
cverr_lasso <- c()
cverr_loglin <- c()
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  
  trainData <- Data[-testIndexes, ]
  trainData_X <- trainData[,-7]
  trainData_Y <- trainData[,7]
  trainX_matrix <- model.matrix(~., Data_X) #change dataX factor variables into dummies
  trainY_matrix <- as.matrix(Data_Y)
  testData <- Data[testIndexes, ]
  testData_X <- testData[,-7]
  testData_Y <- testData[,7]
  testX_matrix <- model.matrix(~., testData_X) #change dataX factor variables into dummies
  testY_matrix <- as.matrix(testData_Y)
  
  model_linear_CV <- lm(charges ~ . , data=trainData )
  
  CV_lasso_CV<-cv.glmnet(trainX_matrix, trainY_matrix, family="gaussian",
                         alpha = 1, nfolds = 10)
  model_lasso_CV <- glmnet(trainX_matrix, trainY_matrix, lambda = CV_lasso_CV$lambda.min, alpha=1)
  
  model_loglinear_CV <- glm(charges~. , data=trainData, family=gaussian(link="log"))
  
  # get errors
  lin_err <- VS_test(model_linear_CV, testData_X, testData_Y)[1]
  lasso_err <- VS_test(model_lasso_CV, testX_matrix, testY_matrix)[1]
  loglin_err <- VS_test(model_loglinear_CV, testData_X, testData_Y)[1]
  cverr_lin <- c(cverr_lin, c(lin_err))
  cverr_lasso <- c(cverr_lasso, c(lasso_err))
  cverr_loglin <- c(cverr_loglin, c(loglin_err))
}

print(mean(cverr_lin))
print(mean(cverr_lasso))
print(mean(cverr_loglin))

# ---- bootstrap ----
train_indices <- sample(nrow(X_matrix), 0.7*nrow(X_matrix), replace = TRUE)
train_X <- X_matrix[train_indices,]
train_Y <- Y_matrix[train_indices,]
test_X <- X_matrix[-train_indices,]
test_Y <- Y_matrix[-train_indices,]

model_linear_VS <- lm(charges ~ . , data= Data[train_indices,] )
CV_lasso_VS<-cv.glmnet(train_X, train_Y, family="gaussian",
                       alpha = 1, nfolds = 10) # use same training data?
model_lasso_VS <- glmnet(train_X, train_Y, lambda = CV_lasso_VS$lambda.min, alpha=1)
model_loglinear_VS <- glm(charges~. , data=Data[train_indices, ], family=gaussian(link="log"))

VS_test <- function(model, testX, testY) {
  y_hat <- predict(model, testX)
  rss <- t(y_hat- testY) %*% (y_hat - testY)
  return(rss/nrow(testX))
}
VS_test(model_linear_VS, Data_X[-train_indices,], Data_Y[-train_indices])
VS_test(model_lasso_VS, test_X, test_Y)
VS_test(model_loglinear_VS, Data_X[-train_indices,], Data_Y[-train_indices])
