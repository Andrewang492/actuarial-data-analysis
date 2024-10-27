library(randomForest) 
library(caret) 
library(tidyverse)



# merged data manip ####
merged <- read_csv("Data/merged.csv")
# turn into factors
merged <- merged %>%mutate(across(
  c(pet_gender, pet_de_sexed_age, nb_address_type_adj, nb_suburb, nb_postcode,
    nb_state, pet_age_years, nb_breed_type, nb_breed_trait),
  as.factor
))
# Make a wait_after_quote variable.
merged <- merged %>% mutate(wait_after_quote = nb_policy_first_inception_date - quote_date) 
#  mutate(wait_after_quote = ifelse(wait_after_quote < 0, 0, wait_after_quote))

#### Doing training ####
set.seed(888)  
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 
data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]

#Define control parameters for train
fitcontrol <- trainControl(method = "cv", 
                           number = 5,
                           savePredictions = TRUE,
                           classProbs = TRUE, 
                           allowParallel = TRUE)

##Random Forest A####
#rf <- train(default ~., data = train, method = "rf", trControl = fitcontrol, tuneLength = 10)
set.seed(248)
# Fit the random forest model
rf_model_a <- randomForest(average_claim_size ~ pet_gender + pet_de_sexed_age + pet_is_switcher + pet_age_months +
                           nb_address_type_adj + nb_state + nb_contribution_excess +
                           owner_age_years + nb_number_of_breeds + nb_average_breed_size +  nb_breed_type + nb_breed_trait + is_multi_pet_plan +
                           quote_date,
                         data = data_train, 
                         ntree = 500, 
                         mtry = 3,
                         importance = TRUE)
# Print a summary of the model
print(rf_model_a)
plot(rf_model_a)
# don't really know what to do with it.
rf_model_a$importance
rf_model_a$importanceSD

##Using caret for rf ####
#set.seed(42)
train_control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)

# rf 1 on all
rf_model <- train(
  average_claim_size ~ pet_gender + pet_de_sexed_age + pet_is_switcher + pet_age_months +
    nb_address_type_adj + nb_state + nb_contribution_excess +
    owner_age_years + nb_number_of_breeds + nb_average_breed_size +  nb_breed_type + nb_breed_trait + is_multi_pet_plan +
    quote_date, 
  data = data_train, 
  method = "rf",
  trControl = train_control,
  tuneLength = 5  # Number of different mtry values to try
)

# Print a summary of the model
print(rf_model)
# View the final model's RMSE on training data
rf_model$results
# Best tuning parameter (mtry)
rf_model$bestTune

# Assuming you have a test data frame called `test_data`
predictions <- predict(rf_model, newdata = data_test)

# Calculate performance metrics
mse <- mean((predictions - data_test$average_claim_size)^2)
rmse <- sqrt(mse)
cat("MSE:", mse, "\nRMSE:", rmse, "\n")
# Feature importance
varImp_rf <- varImp(rf_model, scale = TRUE)
print(varImp_rf)
plot(varImp_rf)
# quote date, petagemonths, owner age years, spitz related, contribution excess, pnscher(but this only has 3 data points),
# 
##try again on another seed (trianing split) ####
set.seed(43)
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 
data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]
train_control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)

rf_model2 <- train(
  average_claim_size ~ pet_gender + pet_de_sexed_age + pet_is_switcher + pet_age_months +
    nb_address_type_adj + nb_state + nb_contribution_excess +
    owner_age_years + nb_number_of_breeds + nb_average_breed_size +  nb_breed_type + nb_breed_trait + is_multi_pet_plan +
    quote_date, 
  data = data_train, 
  method = "rf",
  trControl = train_control,
  tuneLength = 5  # Number of different mtry values to try
)

print(rf_model2)
rf_model2$results
rf_model2$bestTune

predictions2 <- predict(rf_model2, newdata = data_test)

mse2 <- mean((predictions2 - data_test$average_claim_size)^2)
rmse2 <- sqrt(mse2)
cat("MSE:", mse2, "\nRMSE:", rmse2, "\n")
# Feature importance
varImp_rf2 <- varImp(rf_model2, scale = TRUE)
print(varImp_rf2)
plot(varImp_rf2)
# pet age months, quote date. Then maybe owner age years.

##try again for some transformed variables ####
set.seed(40)
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 
data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]
train_control <- trainControl(method = "cv", number = 10, allowParallel = TRUE)

rf_transform <- train(
  average_claim_size ~ pet_gender + pet_de_sexed_age + pet_is_switcher + pet_age_months +
    nb_address_type_adj + nb_state + nb_contribution_excess +
    owner_age_years + nb_number_of_breeds + nb_average_breed_size +  nb_breed_type + nb_breed_trait + is_multi_pet_plan +
    wait_after_quote + I(pet_age_months^2), 
  data = data_train, 
  method = "rf",
  trControl = train_control,
  tuneLength = 10
)

print(rf_transform)
rf_transform$results

predictions3 <- predict(rf_transform, newdata = data_test)

mse3 <- mean((predictions3 - data_test$average_claim_size)^2)
rmse3 <- sqrt(mse3)
cat("MSE:", mse3, "\nRMSE:", rmse3, "\n")
# Feature importance
varImp_rf3 <- varImp(rf_transform, scale = TRUE)
print(varImp_rf3)
plot(varImp_rf3)
# wait after quote is top. pet age months folowed by squared, owner age years.