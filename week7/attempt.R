
# load packages
library(dplyr)
library(randomForest)
library(caret)
library(pROC)
library(ROCR)
library(tidyr)
library(PRROC) #roc.curve
library(rattle) #plotting tree.
# load data
credit <- read.csv("credit.csv")%>% dplyr::select(-X, -ID)
payamt_colsnames <- paste0("PAY_", c(1, 2:6))
credit <- credit%>%dplyr::mutate_at(vars(EDUCATION, MARRIAGE,SEX, default,
                                         payamt_colsnames), funs(factor))
credit$default <- as.factor(ifelse(credit$default == 1, "Yes", "No"))
#Extract a sample from the training set to speed up the computatio
#set.seed(310)
credit <- credit[sample(nrow(credit),size = 1000, replace = FALSE),]
# reproduciblity
set.seed(123)
# data splittings

index <- createDataPartition(credit$default, p = 0.7, list = FALSE)
train <- credit[index, ]; test <- credit[-index, ]

# ---- Model ----
clas <- train(default ~ SEX + EDUCATION + MARRIAGE , data = train, method = "rpart")
predictions <- predict(clas, test)
confusionMatrix(predictions, test$default)


# train a rf
?caret

rfGrid <- expand.grid(mtry=1:5)
ctrl <- trainControl(method = "cv", number = 5)
rffit <- train(x = train[, -c(24)], y = train[, c(24)],
  #default ~ ., 
               data=train,
               method="rf",
               trControl = ctrl,
               tuneGrid = rfGrid,
               ntree = 100,
               maxdepth = 5,
               importance = TRUE)
rffit
plot(rffit)

# train a bag
ctrl <- trainControl(method = "cv", number = 5)
bagfit <- train(x = train[, -c(24)], y = train[, c(24)],
  #default ~ ., 
               data=train,
               method="bag",
               ntree = 100,
               maxdepth = 5,
               importance = TRUE,
  metric = "Accuracy",
  parms = list(split = "gini"))
bagfit
plot(bagfit)

#### CORRECT ANSWERS ####
#Define control parameters for train
fitcontrol <- trainControl(method = "cv", # cv resampling.
                           number = 5, #fold in cv, or for bootstrapping, resampling iterations. # set repeats to do more for CV.
                           savePredictions = TRUE,
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary,
                           allowParallel = TRUE)
####Decision Tree 1####
#CP tuning based Decision Tree
set.seed(319)
# We are tuning cp (complexity) to see how much pruning we should do.
tree1 <- train(default ~., data = train, method = "rpart", metric = "ROC",
               trControl = fitcontrol, tuneLength = 10) # tunelength determines number of searches for appropriate cp.
#tree1 <- train(default ~., data = train, method = "rpart", 
#              trControl = fitcontrol, tuneGrid = expand.grid(cp = seq(0.001,0.005, 0.0005))) # defines what cp are allowed.
print(tree1)
plot(tree1)
#Note the threshold probability for the classification is 50% by default.
tree1_pred <- predict(tree1, newdata = test[,-ncol(credit)], type = "raw") 
tree1_conf <- confusionMatrix(tree1_pred,  test[,ncol(credit)], positive="Yes")

tree1_probpred <- predict(tree1, newdata = test[,-ncol(credit)], type = "prob")
tree1_auc <- roc.curve(scores.class0 = tree1_probpred$Yes, 
                       weights.class0 = as.numeric(test$default)-1, curve = T)
tree1_auc$auc

fancyRpartPlot(tree1$finalModel,palettes="RdPu") #For tree1
####Decision Tree 2####
#Max tree depth tuning based Decision Tree
set.seed(192)
tree2 <- train(default ~., data = train, method = "rpart2", metric = "ROC",
               trControl = fitcontrol, tuneLength = 10)

print(tree2)
plot(tree2)

tree2_pred <- predict(tree2, newdata = test[,-ncol(credit)], type = "raw")
tree2_conf <- confusionMatrix(tree2_pred,  test[,ncol(credit)], positive="Yes")
tree2_conf

tree2_probpred <- predict(tree2, newdata = test[,-ncol(credit)], type = "prob")
tree2_auc <- roc.curve(scores.class0 = tree2_probpred$Yes, 
                       weights.class0 = as.numeric(test$default)-1, curve = T)
tree2_auc$auc

# plots for both decision trees.
fancyRpartPlot(tree2$finalModel,sub="",palettes="RdPu") #For tree2

####Bagging####
#no tuning parameters to tune.
set.seed(643)
bag <- train(default ~., data = train, method = "treebag", metric = "ROC",
             trControl = fitcontrol)
print(bag)

bag_pred <- predict(bag, newdata = test[,-ncol(credit)], type = "raw")
bag_conf <- confusionMatrix(bag_pred,  test[,ncol(credit)], positive="Yes")

bag_probpred <- predict(bag, newdata =  test[,-ncol(credit)], type = "prob")
bag_auc <- roc.curve(scores.class0 = bag_probpred$Yes, 
                     weights.class0 = as.numeric(test$default)-1, curve = T)
bag_auc$auc

#### force some tuning in bagging: ####
#tuning maxdepth, minsplit. Could do minbucket for size of terminal nodes.
tuning_grid <- expand.grid(
  maxdepth = c(1, 3, 5, 8, 15),
  minsplit = c(2, 5, 10, 15), #(minimum number to consider splitting.)
  ROC = NA,
  Sens = NA,
  Spec = NA
)

# Execute full cartesian grid search...
set.seed(761)

for(i in seq_len(nrow(tuning_grid))) {
  
  # Fit model for each hyperparameter combination...
  fit <- train(default ~., data = train, method = "treebag", metric = "ROC",
               trControl = fitcontrol,
               maxdepth = tuning_grid$maxdepth[i],
               minsplit = tuning_grid$minsplit[i])
  
  
  # Save fit metrics from each model...
  tuning_grid$ROC[i] <- fit$results$ROC
  tuning_grid$Sens[i] <- fit$results$Sens
  tuning_grid$Spec[i] <- fit$results$Spec
}

# Assess top 10 models...
tuning_grid %>%
  arrange(-ROC) %>%
  head(10)

best_ROC <- tuning_grid[tuning_grid$ROC == max(tuning_grid$ROC), ]
# minimal performance increase by restricting minsplit and amxdepth.

#Please update the following based on to the run of the previous R chunk
best_ROC <- expand.grid(
  maxdepth = 3, 
  minsplit = 15
)

# Re-run model with best hyperparameters...
set.seed(889)
bag_best <- train(default ~., data = train, method = "treebag", metric = "ROC",
                  trControl = fitcontrol,
                  maxdepth = best_ROC$maxdepth,
                  minsplit = best_ROC$minsplit)

print(bag_best)

bag_best_pred <- predict(bag_best, newdata = test[,-ncol(credit)], type = "raw")
bag_best_conf <- confusionMatrix(bag_best_pred,  test[,ncol(credit)], positive="Yes")
# setting positive as "Yes" is important, otherwise, you may get wrong a conclusion.

bag_best_probpred <- predict(bag_best, newdata =  test[,-ncol(credit)], type = "prob")
bag_best_auc <- roc.curve(scores.class0 = bag_best_probpred$Yes, 
                          weights.class0 = as.numeric(test$default)-1, curve = T)
bag_best_auc$auc

#### Random Forest ####
# Tunes mtry (how many predictors to choose from, usually sqrt(p) in classif or p/3 in regres)
# set.seed(248)
rf <- train(default ~., data = train, method = "rf", metric = "ROC",
            trControl = fitcontrol, tuneGrid = expand.grid(mtry = seq(1,15,1)))
print(rf)
plot(rf)

rf_pred <- predict(rf, newdata = test[,-ncol(credit)], type = "raw")
rf_conf <- confusionMatrix(rf_pred,  test[,ncol(credit)], positive="Yes")

rf_probpred <- predict(rf, newdata =  test[,-ncol(credit)], type = "prob")
rf_auc <- roc.curve(scores.class0 = rf_probpred$Yes, 
                    weights.class0 = as.numeric(test$default)-1, curve = T)
rf_auc$auc

#### Perofrmance comparison 1 ####
#Find F-scores for each fitted model
tree1_fscore <- (2*tree1_conf$byClass["Sensitivity"]*tree1_conf$byClass["Pos Pred Value"])/(tree1_conf$byClass["Sensitivity"] + tree1_conf$byClass["Pos Pred Value"])
tree2_fscore <- (2*tree2_conf$byClass["Sensitivity"]*tree2_conf$byClass["Pos Pred Value"])/(tree2_conf$byClass["Sensitivity"] + tree2_conf$byClass["Pos Pred Value"])
bag_fscore <- (2*bag_conf$byClass["Sensitivity"]*bag_conf$byClass["Pos Pred Value"])/(bag_conf$byClass["Sensitivity"] + bag_conf$byClass["Pos Pred Value"])
best_bag_fscore <- (2*bag_best_conf$byClass["Sensitivity"]*bag_best_conf$byClass["Pos Pred Value"])/(bag_best_conf$byClass["Sensitivity"] + bag_best_conf$byClass["Pos Pred Value"])
rf_fscore <- (2*rf_conf$byClass["Sensitivity"]*rf_conf$byClass["Pos Pred Value"])/(rf_conf$byClass["Sensitivity"] + rf_conf$byClass["Pos Pred Value"])

# Create a data frame to store the results
model_comparison <- data.frame(
  Model = c("Tree 1", "Tree 2", "Bagging", "Tuned Bagging","Random Forest"),
  Accuracy = c(tree1_conf$overall['Accuracy'], tree2_conf$overall['Accuracy'], bag_conf$overall['Accuracy'], bag_best_conf$overall['Accuracy'], rf_conf$overall['Accuracy']),
  AUC = c(tree1_auc$auc, tree2_auc$auc, bag_auc$auc, bag_best_auc$auc, rf_auc$auc),
  Sensitivity = c(tree1_conf$byClass['Sensitivity'], tree2_conf$byClass['Sensitivity'], bag_conf$byClass['Sensitivity'], bag_best_conf$byClass['Sensitivity'], rf_conf$byClass['Sensitivity']),
  Specificity = c(tree1_conf$byClass['Specificity'], tree2_conf$byClass['Specificity'], bag_conf$byClass['Specificity'], bag_best_conf$byClass['Specificity'], rf_conf$byClass['Specificity']),
  F_score = c(tree1_fscore, tree2_fscore, bag_fscore, best_bag_fscore, rf_fscore)
)

# Print the data frame
knitr::kable(model_comparison, caption = "Evaluation of different models using AUC, accuracy, sensitivity, specificity and F-score.")


#### OOB vs TEst set ####
# create training and validation data 
# At what time is this??? During training at each of finding many hundred trees?

set.seed(123)

index <- createDataPartition(train$default, p = 0.75, list = FALSE) 

# train & validation sets

trainv <- train[index, ]; validv <- train[-index, ]

# random forest 

random_oob <- randomForest(formula = default ~., data = trainv, xtest = validv[,-24], 
                           ytest   = validv$default, mtry = 7) #use optimal mtry

# ntree=500 by default, Number of trees to grow. 
# This should not be set to too small a number, to ensure that every input row 
# gets predicted at least a few times.

# extract OOB & validation errors
oob <- random_oob$err.rate[,1]
validation <- random_oob$test$err.rate[,1]

# compare error rates
tibble::tibble(`Out of Bag Error` = oob, `Test error` = validation,
               ntrees = 1:random_oob$ntree)%>%gather(Metric, Error, -ntrees) %>%
  ggplot(aes(ntrees, Error, color = Metric)) +  geom_line() + 
  xlab("Number of trees")
