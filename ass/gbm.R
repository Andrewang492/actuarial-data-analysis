library(gbm)
library(caret) 


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
              "pet_de_sexed", "breed_group", "total_earned_units", "wait_after_quote")
predictors <- predictors[!(predictors %in% excluded) & !grepl("^condition", predictors)]

# remove the ones without a claim amount
merged <- merged %>% filter(average_claim_size > 0) %>%
  group_by(nb_breed_trait) %>%
  filter(n() > 5) %>%
  ungroup()
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 

train_data <- merged[train_indices, ]
test_data = merged[-train_indices, ]

## GBM1
n_trees <- 500          # Number of trees
interaction_depth <- 3  # Depth of each tree
shrinkage <- 0.001       # Learning rate
min_obs <- 10           # Minimum number of observations per node


# Modelling Claim Paid
sev_model_gbm_raw_claim <- as.formula(paste("average_claim_size ~", paste(predictors, collapse = " + ")))

severity_model <- gbm(
  formula = sev_model_gbm_raw_claim,
  distribution = "gaussian",  # Gaussian distribution for severity (continuous)
  data = train_data,
  n.trees = n_trees,
  interaction.depth = interaction_depth,
  shrinkage = shrinkage,
  n.minobsinnode = min_obs,
  cv.folds = 5,             # Cross-validation folds
  verbose = FALSE           # Suppress output
)

train_pred <- predict(severity_model, newdata = train_data, n.trees = n_trees)
test_pred <- predict(severity_model, newdata = test_data, n.trees = n_trees)

train_rmse <- sqrt(mean((train_data$average_claim_size - train_pred)^2))
test_rmse <- sqrt(mean((test_data$average_claim_size - test_pred)^2))

cat("Training RMSE:", train_rmse, "\n")
cat("Test RMSE:", test_rmse, "\n")

summary(severity_model, n.trees = n_trees)  # Uses optimal number of trees

# compare to a glm:
m4 <- glm(average_claim_size ~ pet_gender + pet_de_sexed_age  +  
            pet_is_switcher + pet_age_months + nb_contribution + nb_excess +
            nb_address_type_adj,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = train_data)
glmpredictions <- predict.glm(m4, newdata = test_data)
cat("RMSE:", sqrt(mean((glmpredictions-test_data$average_claim_size)^2)))
