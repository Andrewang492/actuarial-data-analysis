library(randomForest) 
library(caret) 
library(tidyverse)

# merged data manip ####
merged <- read_csv("Data/merged.csv")
# Make a wait_after_quote variable.
merged <- merged %>% mutate(wait_after_quote = nb_policy_first_inception_date - quote_date) 
# turn into factors
merged <- merged %>%mutate(across(
  c(pet_gender, pet_de_sexed_age, nb_address_type_adj, nb_suburb, nb_postcode,
    nb_state, pet_age_years, nb_breed_type, nb_breed_trait, pet_is_switcher),
  as.factor
))
merged <- merged %>%mutate(
  across(c(pet_gender, pet_de_sexed, pet_de_sexed_age, pet_is_switcher, 
           nb_address_type_adj, nb_suburb, nb_postcode,
           nb_state, nb_breed_type,
           nb_breed_trait, is_multi_pet_plan, nb_breed_name_unique,
           nb_breed_name_unique_concat, quote_time_group, breed, breed_group), 
         as.factor)
)

# NOTE change dates to numerics here.
merged <- merged %>%mutate(
  across(c(nb_policy_first_inception_date, person_dob, quote_date, UW_Date, wait_after_quote), 
         as.numeric)
)

# Make a wait_after_quote variable.
merged <- merged %>% mutate(wait_after_quote = as.numeric(nb_policy_first_inception_date - quote_date))

#Modelling ####
set.seed(88)  

predictors <- colnames(merged)
excluded <- c("total_claim_amount", "claim_paid", "claimNb", "person_dob", "exposure_id", "average_claim_size", "nb_policy_first_inception_date", "nb_postcode", "nb_suburb", "nb_breed_type", "nb_breed_name_unique",  "nb_breed_name_unique_concat", "max_tenure", "UW_Date", "breed", "nb_breed_trait", "pet_age_years",
              "pet_de_sexed", "breed_group", "total_earned_units", "quote_date")
predictors <- predictors[!(predictors %in% excluded) & !grepl("^condition", predictors)]

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

# compare to a glm:
m4 <- glm(average_claim_size ~ pet_gender + pet_de_sexed + pet_de_sexed_age  +  
  pet_is_switcher + pet_age_months + nb_contribution + nb_excess +
  nb_address_type_adj + nb_contribution_excess + owner_age_years + 
  nb_number_of_breeds + nb_average_breed_size + is_multi_pet_plan + 
  breed_group,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train %>% filter(average_claim_size > 0))
glmpredictions <- predict.glm(m4, newdata = data_test)
cat("RMSE:", sqrt(mean((glmpredictions-data_test$average_claim_size)^2)))

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
# wait after quote is top. pet age months followed by squared, owner age years.