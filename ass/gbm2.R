library(gbm)
library(caret) 
library(tidyverse)

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
dates_to_numeric = function(d) {
  return(as.numeric(as.Date(d) - as.Date(min(d))))
}
merged <- merged %>%mutate(
  across(c(nb_policy_first_inception_date, person_dob, quote_date, UW_Date), 
         dates_to_numeric)
)
# more numerics
merged <- merged %>% mutate(wait_after_quote = as.numeric(wait_after_quote))

#Modelling ####
set.seed(88)  

predictors <- colnames(merged)
excluded <- c("total_claim_amount", "claim_paid", "claimNb", "person_dob", "exposure_id", "average_claim_size", "nb_policy_first_inception_date", "nb_postcode", "nb_suburb", "nb_breed_type", "nb_breed_name_unique",  "nb_breed_name_unique_concat", "max_tenure", "UW_Date", "breed", "nb_breed_trait", "pet_age_years",
              "pet_de_sexed", "breed_group", "total_earned_units", "quote_date")
predictors <- predictors[!(predictors %in% excluded) & !grepl("^condition", predictors)]
# get another set of predictors based on sample output. (these will certainly be there for pricing)
sampleout <- read_csv("Data/Sample_price_output_file.csv")
predictors2 <- colnames(sampleout)
excluded2 <- c("earned_premium", "earned_units", "exposure_id", "exposure_id_1", "row_num") #remove unusable
# remove bad predictors. suburb and postcode make best trees since splits are 1 or the rest?. breed_concat is not interpretable.
excluded2 <- c(excluded2, "nb_suburb", "nb_postcode", "nb_breed_name_unique_concat",
               "nb_breed_name_unique", "nb_breed_trait", # unique breed is also too hard to use. Breed trait is all good, show me something else.
               "pet_age_years", "person_dob") # years correlated. Dob correlated.
predictors2 <- predictors2[!(predictors2 %in% excluded2)]

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
interaction_depth <- 2  # Depth of each tree
shrinkage <- 0.001       # Learning rate
min_obs <- 10           # Minimum number of observations per node


# Modelling across output_csv predictors
gbm1formula <- as.formula(paste("average_claim_size ~", paste(predictors, collapse = " + "))) # USING PREDICTORS2 !!!!!!!!

severity_model <- gbm(
  formula = gbm1formula,
  distribution = "gaussian",  # Gaussian distribution for severity (continuous)
  data = train_data,
  n.trees = n_trees,
  interaction.depth = interaction_depth,
  shrinkage = shrinkage,
  n.minobsinnode = min_obs,
  cv.folds = 5,             # Cross-validation folds
  verbose = FALSE           # Suppress output
)

train_pred <- predict(severity_model, train_data, n.trees = n_trees)
test_pred <- predict(severity_model, test_data, n.trees = n_trees)

train_rmse <- sqrt(mean((train_data$average_claim_size - train_pred)^2))
test_rmse <- sqrt(mean((test_data$average_claim_size - test_pred)^2))

cat("Training RMSE:", train_rmse, "\n")
cat("Test RMSE:", test_rmse, "\n")

summary(severity_model, n.trees = n_trees)  # Uses optimal number of trees

# I Think that basically both ages are good. Also breed is helpful but it might 
# be because its levels are really good for trees, maybe there is some overfitting.
# The suburb and postcode would be the strongest because of their singleton splits.
# This was all under 3 depth.
# 
# Same results at 1 depth except no owner age at all. Pet age is alright.
# At when breed was also removed, age top, breed trait cam up, owner age became useful.
#
# When  depth 2, owner age top, then pet age, breed trait, and contribution + excess.

# ^ above notes wrong, they made some numerics factors.
# breed trait, quote time group, contribution excess, pet age.
