library(ggplot2)
library(tidyverse)
library(MLmetrics) #rmse
library(statmod) # AIC
library(caret)
library(mgcv) #Gam

#Data manip ####
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

# fix up person dob:
merged <- merged %>%
  mutate(person_dob = ifelse(is.na(person_dob), as.Date(today() - years(owner_age_years)), person_dob)) %>%
  mutate(person_dob = as.Date(person_dob))

# change dates to numerics here.
dates_to_numeric = function(d) {
  return(as.numeric(as.Date(d) - as.Date(min(d))))
}

merged <- merged %>%mutate(
  across(c(nb_policy_first_inception_date, person_dob, quote_date, UW_Date), 
         dates_to_numeric)
)
# more numerics
merged <- merged %>% mutate(wait_after_quote = as.numeric(wait_after_quote))
# remove the ones without a claim amount
merged <- merged %>% filter(average_claim_size > 0) # get rid of like 97
# remove outliers
#merged <- merged %>% filter(average_claim_size < 5001) 
# Move some traits since small traits will ruin training. Move into unknown
merged <- merged %>%
  mutate(nb_breed_trait = as.character(nb_breed_trait)) %>%
  group_by(nb_breed_trait) %>%
  mutate(nb_breed_trait = ifelse(n() < 5, "unknown", nb_breed_trait)) %>% 
  ungroup() %>% # get rid of 3
  mutate(nb_breed_trait = as.factor(nb_breed_trait))


#Data split ####
set.seed(43)  
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 
data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]


# Test functions ####


spearmancor_L <- function(model, testdata) {
  predicted_values <- exp(predict(model, newdata = testdata, type = "response")) # use predict.glm?
  observed_values <- testdata$average_claim_size
  cor <- cor(predicted_values, observed_values, method = "spearman")
  return(cor)
}

## CV ####
# Set up cross-validation folds
folds = 5
set.seed(123)  # for reproducibility
merged$folds <- sample(1:folds, nrow(merged), replace = TRUE)
cross_validate_glm_l <- function(data, formula, family, folds = 5) {
  # Initialize a list to store the mean squared error for each fold
  rmse_values <- numeric(folds)

  # Loop through each fold
  for (i in 1:folds) {
    # Split data into training and test sets
    train_data <- data[data$folds != i, ]
    test_data <- data[data$folds == i, ]
    
    # Fit the GLM model on the training set
    model <- glm(formula = formula, 
                 family = family, 
                 weight = claimNb, 
                 data = train_data)
    
    # Make predictions on the test set
    predictions <- exp(predict(model, newdata = test_data, type = "response"))
    
    # Calculate mean squared error on the test set
    observed_values = test_data$average_claim_size
    rmse_values[i] <- sqrt(mean((observed_values - predictions)^2))
  }
  
  # Calculate the average MSE across all folds
  avg_rmse <- mean(rmse_values)

  # Return the average MSE and MSE for each fold
  list(average_rmse = avg_rmse,
       mse_per_fold = rmse_values)
}

deciletest_L <- function(model, testdata) {
  dec_test <- testdata
  
  dec_test$predicted_average_claim_size <- exp(predict(model, dec_test, type = "response")) # Put your model in here
  
  dec_test$decile <- ntile(dec_test$predicted_average_claim_size, 10)
  
  # Calculate mean actual and predicted claim size for each decile
  decile_summary <- dec_test %>%
    group_by(decile) %>%
    summarise(
      mean_actual = mean(average_claim_size, na.rm = TRUE),
      mean_predicted = mean(predicted_average_claim_size, na.rm = TRUE)
    )
  
  # Plot
  myplot <- ggplot(decile_summary, aes(x = decile)) +
    geom_line(aes(y = mean_actual, color = "Actual"), linewidth = 1.2) +
    geom_line(aes(y = mean_predicted, color = "Predicted"), linewidth = 1.2) +
    labs(title = "Decile Plot Actual vs. Predicted Avg. Claim Size",
         x = "Decile",
         y = "Mean Claim Size") +
    scale_color_manual(name = "Legend", values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_minimal()
  myplot
}

glmdiagnosis_L <- function(m, test_data = data_test) {
  # residual size plot
  par(mfrow=c(2,2))
  d.residuals <- residuals(m, type="deviance")
  plot(d.residuals)
  abline(a = 0, b = 0, col = "red")
  # residual hist
  hist(d.residuals, breaks = 100)
  # Create a Q-Q plot of the deviance residuals
  qqnorm(d.residuals, main = "Q-Q Plot of Deviance Residuals")
  qqline(d.residuals, col = "red", lwd = 2)  # Add a reference line
  
  
  predictions <- exp(predict(m, newdata = test_data, type="response"))
  expected <- test_data$average_claim_size
  print(anova(m))
  print(summary(m))
  print(data.frame(row.names = c("RMSE", "AIC", "BIC", "Model Dev", "Spearman Cor"), result = c(
    RMSE(predictions, test_data$average_claim_size),
    AIC(m),
    BIC(m),
    deviance(m),
    spearmancor_L(m, test_data)
  )))
  
  ggplot() +
    geom_density(data = data.frame(predictions = predictions),
                 mapping = aes(x=predictions)) +
    labs(title = "x") +
    geom_density(data = test_data, mapping = aes(x=average_claim_size), col = 'red')
}

# Models ####
f1l <- formula(log(average_claim_size) ~ 1)
m1l <- glm(f1l,
          family = "gaussian", 
          weights = claimNb, 
          data = data_train)
summary(m1l)
cross_validate_glm_l(merged, f1l, "gaussian")
deciletest_L(m1l, data_test)


f2l <- formula(log(average_claim_size) ~ pet_de_sexed_age + nb_contribution + nb_excess +
                nb_breed_trait)
m2l <- glm(f2l,
          family = "gaussian", 
          weights = claimNb, 
          data = data_train)
summary(m2l)
cross_validate_glm_l(merged, f2l, "gaussian")
deciletest_L(m2l, data_test)

f3l <- formula(log(average_claim_size)  ~ pet_gender + pet_de_sexed + pet_de_sexed_age  +  
                 pet_is_switcher + pet_age_months + nb_contribution + nb_excess + nb_address_type_adj +
                 nb_contribution_excess + owner_age_years + nb_number_of_breeds + nb_average_breed_size +
                 is_multi_pet_plan + nb_breed_trait + quote_time_group + wait_after_quote)
m3l <- glm(f3l,
          family = "gaussian", 
          weights = claimNb, 
          data = data_train)
summary(m3l)
cross_validate_glm_l(merged, f3l, "gaussian")
deciletest_L(m3l, data_test)
glmdiagnosis_L(m3l)

f4l <- formula(log(average_claim_size)  ~  pet_de_sexed_age  +  
                 pet_age_months + nb_contribution + nb_excess +
                 nb_average_breed_size + nb_breed_trait + quote_time_group + wait_after_quote)
m4l <- glm(f4l,
          family = "gaussian", 
          weights = claimNb, 
          data = data_train)
summary(m4l)
cross_validate_glm_l(merged, f4l, "gaussian")
deciletest_L(m4l, data_test)
glmdiagnosis_L(m4l)

f10l <- formula(log(average_claim_size+1) ~ pet_de_sexed_age +
                 nb_contribution + nb_excess + log(pet_age_months) +
                 Mean_income)
m10l <- glm(f10l,
           family = Gamma(link="log"), 
           weights = claimNb, 
           data = merged)
glmdiagnosis_L(m10l)
summary(m10l)
cross_validate_glm_l(merged, f10l, "gaussian")
deciletest_L(m10l, data_test)
