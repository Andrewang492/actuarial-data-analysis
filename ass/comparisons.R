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

merged <- merged %>% select (-c(5:15)) # get rid of conditions values.

# put some income data in
incomepostcode4 <- read_csv("Data/External/income_by_postcode_sa4.csv")
incomepostcode4$POSTCODE <- as.factor(incomepostcode4$POSTCODE)
merged <- merged %>% left_join( y = incomepostcode4, by = join_by(nb_postcode == POSTCODE))
merged <- merged %>%
  mutate(across(38:43, ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) # change 11 unmatched postcodes to be average.

# put some land data in
landpostcode4 <- read_csv("Data/External/land_by_postcode_sa4.csv")
landpostcode4$POSTCODE <- as.factor(landpostcode4$POSTCODE)
merged <- merged %>% left_join( y = landpostcode4, by = join_by(nb_postcode == POSTCODE))
merged <- merged %>%
  mutate(across(44:63, ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) # change 11 unmatched postcodes to be average.
#merged %>% filter(nb_postcode == 3336) %>% select(c(38:43), exposure_id)
merged %>% filter(nb_postcode == 3336) %>% select(c(44:54), exposure_id)
#Data split ####
set.seed(43)  
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 
data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]

# Test functions ####

gammadeviance <- function(model, testdata) {
  predicted_values <- predict(model, newdata = testdata, type = "response") # use predict.glm?
  observed_values <- testdata$average_claim_size
  deviance <- 2 * sum(-log(observed_values / predicted_values) + 
                        (observed_values - predicted_values)/predicted_values)
  return(deviance)
}

spearmancor <- function(model, testdata) {
  predicted_values <- predict(model, newdata = testdata, type = "response") # use predict.glm?
  observed_values <- testdata$average_claim_size
  cor <- cor(predicted_values, observed_values, method = "spearman")
  return(cor)
}
# Set up cross-validation folds
folds = 5
set.seed(123)  # for reproducibility
merged$folds <- sample(1:folds, nrow(merged), replace = TRUE)
cross_validate_glm <- function(data, formula, family, folds = 5) {
  # Initialize a list to store the mean squared error for each fold
  rmse_values <- numeric(folds)
  dev_values <- numeric(folds) # Gamma deviance
  
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
    predictions <- predict(model, newdata = test_data, type = "response")
    
    # Calculate mean squared error on the test set
    observed_values = test_data$average_claim_size
    rmse_values[i] <- RMSE(predictions, test_data$average_claim_size, na.rm = TRUE)
    dev_values[i] <- 2 * sum(-log(observed_values / predictions) + 
                         (observed_values - predictions)/predictions)
  }
  
  # Calculate the average MSE across all folds
  avg_rmse <- mean(rmse_values)
  avg_dev <- mean(dev_values)
  
  # Return the average MSE and MSE for each fold
  list(average_rmse = avg_rmse, average_dev = avg_dev, 
       mse_per_fold = rmse_values, gammadev_per_fold = dev_values)
}

deciletest <- function(model, testdata) {
  dec_test <- testdata
  
  dec_test$predicted_average_claim_size <- predict(model, dec_test, type = "response") # Put your model in here
  
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

glmdiagnosis <- function(m, test_data = data_test) {
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
  

  predictions <- predict(m, newdata = test_data, type="response")
  expected <- test_data$average_claim_size
  print(anova(m))
  print(summary(m))
  results <- data.frame(row.names = c("RMSE", "AIC", "BIC", "Model Dev", "Spearman Cor"), result = c(
    RMSE(predictions, test_data$average_claim_size, na.rm = TRUE),
    AIC(m),
    BIC(m),
    deviance(m),
    spearmancor(m, test_data)
  ))
  print(results)
  
  ggplot() +
    geom_density(data = data.frame(predictions = predictions),
                 mapping = aes(x=predictions)) +
    labs(title = "x") +
    geom_density(data = test_data, mapping = aes(x=average_claim_size), col = 'red')
  return(results)
}


#Models ####
## Null model ####
nullmodel <- glm(average_claim_size ~ 1,
                 family = Gamma(link = "log"), 
                 weights = claimNb, 
                 data = data_train)
summary(nullmodel)
gammadeviance(nullmodel, data_test)
cross_validate_glm(data = merged, formula = formula(average_claim_size ~ 1), family=Gamma(link="log"))

## Jayden ####
f1 <- formula(average_claim_size ~ pet_gender + pet_de_sexed + pet_de_sexed_age  +  
                pet_is_switcher + pet_age_months + nb_contribution + nb_excess + nb_address_type_adj +
                nb_contribution_excess + owner_age_years + nb_number_of_breeds + nb_average_breed_size +
                is_multi_pet_plan + breed_group)
m1 <- glm(f1,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
summary(m1)
gammadeviance(m1, data_test)
cross_validate_glm(merged, f1, Gamma(link="log"))

## Lognormal ####
f2 <- formula(log(average_claim_size) ~ pet_de_sexed_age + nb_contribution + nb_excess +
  breed_group)
m2 <- glm(f2,
          family = "gaussian", 
          weights = claimNb, 
          data = data_train)
summary(m2)
cross_validate_glm(merged, f2, "gaussian")

## Gamma with 7. m3 ####
f3 <- formula(average_claim_size ~ pet_age_months + pet_de_sexed_age + nb_contribution +
                nb_excess + breed_group + nb_breed_type)
m3 <- glm(f3,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
gammadeviance(m3, data_test)
cross_validate_glm(merged, f3, Gamma(link="log"))

## Gamma with 4. m4 ####
f4 <- formula(average_claim_size ~ pet_de_sexed_age + nb_contribution + nb_excess +
                nb_breed_trait)
m4 <- glm(f4,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
gammadeviance(m4, data_test)
cross_validate_glm(merged, f4, Gamma(link="log"))
## Gamma with many variables and some transformations ####
f5 <- formula(average_claim_size ~ pet_de_sexed_age  +  
                pet_is_switcher + pet_age_months + nb_contribution + nb_excess + nb_number_of_breeds +
                breed_group + pet_age_months*owner_age_years +
                wait_after_quote * pet_age_months + I(owner_age_years ^2))
m5 <- glm(f5,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
summary(m5)
gammadeviance(m5, data_test)
cross_validate_glm(merged, f5, Gamma(link="log"))

## smaller gamma model than previous. ####
f6 <- formula(average_claim_size ~ pet_de_sexed_age  +  
                pet_age_months + nb_contribution + nb_excess +
                breed_group + wait_after_quote + I(owner_age_years ^2))
m6 <- glm(f6,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m6, data_test)
deciletest(m6, data_test)
## my favourite ####
f7 <- formula(average_claim_size ~ pet_de_sexed_age +
                 nb_contribution + nb_excess + wait_after_quote + pet_age_months)
m7 <- glm(f7,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m7, data_test)
deciletest(m7, data_test)
gammadeviance(m7, data_test)
cross_validate_glm(merged, f7, Gamma(link="log"))
## GAM ####
f9 <- formula(average_claim_size ~ pet_de_sexed_age  + nb_average_breed_size +
                pet_age_months + nb_contribution + nb_excess + s(wait_after_quote))
m9 <- gam(f9,
          family = Gamma(link = "log"), 
          weights = claimNb,
          data = data_train)
summary(m9)
glmdiagnosis(m9, data_test)
deciletest(m9, data_test)
gammadeviance(m9, data_test)
#cross_validate_glm(merged, f9, Gamma(link="log"))

## breed group fav instead of breed trait ####
f8 <- formula(average_claim_size ~ pet_de_sexed_age  + nb_average_breed_size +
                pet_age_months + nb_contribution + nb_excess + quote_time_group +
                breed_group)
m8 <- glm(f8,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m8, data_test)
deciletest(m8, data_test)
gammadeviance(m8, data_test)
cross_validate_glm(merged, f8, Gamma(link="log"))


## with external income Fav ####
f10 <- formula(average_claim_size ~ pet_de_sexed_age +
                nb_contribution + nb_excess + wait_after_quote + pet_age_months +
                Employee_earners + Employee_median_age + Total_income_m + 
                Mean_income + Median_income + Main_income_pct)
f10 <- formula(average_claim_size ~ pet_de_sexed_age +
                nb_contribution + nb_excess + quote_time_group + pet_age_months +
                Total_income_m)
f10 <- formula(average_claim_size ~ pet_de_sexed_age +
                nb_contribution + nb_excess + wait_after_quote + pet_age_months +
                Employee_earners)
m10 <- glm(f10,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m10, data_test)
deciletest(m10, data_test)
gammadeviance(m10, data_test)
cross_validate_glm(merged, f10, Gamma(link="log"))
## with external land ####
# Fail with most.
f12 <- formula(average_claim_size ~ pet_de_sexed_age +
                 nb_contribution + nb_excess + log(pet_age_months) +
                 Mean_income + I((irrigated_agricultural_land_area_ha)))
m12 <- glm(f12,
           family = Gamma(link = "log"), 
           weights = claimNb, 
           data = merged)
glmdiagnosis(m12, data_test)
deciletest(m10, data_test)
gammadeviance(m10, data_test)
cross_validate_glm(merged, f10, Gamma(link="log"))
# Main ####

glmdiagnosis(nullmodel, data_test)
deciletest(nullmodel, data_test)
cross_validate_glm(merged, formula(average_claim_size ~ 1), Gamma(link="log"))
gammadeviance(nullmodel, data_test)

f10 <- formula(average_claim_size ~ pet_de_sexed_age +
                 nb_contribution + nb_excess + log(pet_age_months) +
                 Mean_income + nb_breed_trait)
m10 <- glm(f10,
           family = Gamma(link = "log"), 
           weights = claimNb, 
           data = data_train)
m10r <- glmdiagnosis(m10, data_test)
deciletest(m10, data_test)
gammadeviance(m10, data_test)
cross_validate_glm(merged, f10, Gamma(link="log"))

glmdiagnosis(m1, data_test)
deciletest(m1, data_test)
cross_validate_glm(merged, f1, Gamma(link="log"))
gammadeviance(m1, data_test)

glmdiagnosis(m2, data_test)
deciletest(m2, data_test)
cross_validate_glm(merged, f2, Gamma(link="log"))
gammadeviance(m2, data_test)


glmdiagnosis(m7, data_test)
deciletest(m7, data_test)
cross_validate_glm(merged, f7, Gamma(link="log"))
gammadeviance(m7, data_test)


# Final !?####

f10 <- formula(average_claim_size ~ pet_de_sexed_age +
                 nb_contribution + nb_excess + log(pet_age_months) +
                 Mean_income + nb_breed_trait)
m10 <- glm(f10,
           family = Gamma(link = "log"), 
           weights = claimNb, 
           data = merged)
m10r <- glmdiagnosis(m10, data_test)
m10r
deciletest(m10, data_test)
gammadeviance(m10, data_test)
cross_validate_glm(merged, f10, Gamma(link="log"))


# compare with subset. It's better but i don't want to change.
f10subset <- formula(average_claim_size ~ pet_de_sexed_age +
                 nb_contribution + nb_excess + log(pet_age_months)) # without mean income
m10subset <- glm(f10subset,
           family = Gamma(link = "log"), 
           weights = claimNb, 
           data = merged)
anova(m10subset, m10, test = "LRT")

# Electing to include mean income just because.
# Considering wait_after_quote because gbm output.

# Gathering results to present ####
m0r <- glmdiagnosis(nullmodel, data_test)
deciletest(nullmodel, data_test)
m0cv <- cross_validate_glm(merged, formula(average_claim_size ~ 1), Gamma(link="log"))
gammadeviance(nullmodel, data_test)

m1r <- glmdiagnosis(m1, data_test)
deciletest(m1, data_test)
m1cv <- cross_validate_glm(merged, f1, Gamma(link="log"))
gammadeviance(m1, data_test)

m4r <- glmdiagnosis(m4, data_test)
deciletest(m4, data_test)
m4cv <- cross_validate_glm(merged, f4, Gamma(link="log"))
gammadeviance(m4, data_test)

m10r <- glmdiagnosis(m10, data_test)
deciletest(m10, data_test)
m10cv <- cross_validate_glm(merged, f10, Gamma(link="log"))
gammadeviance(m10, data_test)

m12r <- glmdiagnosis(m12, data_test)
deciletest(m12, data_test)
m12cv <- cross_validate_glm(merged, f12, Gamma(link="log"))
gammadeviance(m12, data_test)

ress <- data.frame(m0r, m1r, m4r, m10r, m12r)
names(ress) <- c("m0", "m1", "m4", "m10", "m12")

m12cv[c('average_rmse', 'average_dev')]
cvs <- data.frame(rownames = c("CV RMSE", "CV Gamma Deviance"),
                  c(m0cv$average_rmse, m0cv$average_dev), 
                  c(m1cv$average_rmse, m1cv$average_dev),
                  c(m4cv$average_rmse, m4cv$average_dev),
                  c(m10cv$average_rmse, m10cv$average_dev),
                  c(m12cv$average_rmse, m12cv$average_dev))
names(cvs) <- c(" ", "m0", "m1", "m4", "m10", "m12")

ress
cvs
