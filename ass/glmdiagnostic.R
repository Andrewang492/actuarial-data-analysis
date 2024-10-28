library(ggplot2)
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

hist(merged$average_claim_size, breaks = 100)
plot(density(merged$average_claim_size, breaks = 100))


#Modelling ####
set.seed(888)  
# remove the ones without a claim amount
merged <- merged %>% filter(average_claim_size > 0) %>%
  group_by(nb_breed_trait) %>%
  filter(n() > 5) %>%
  ungroup()
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 

data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]

## gamma
m7 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + quote_time_group, 
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
summary(m7)
par(mfrow = c(2, 2))
plot(m7)
# Plot observed vs. fitted values
plot(m7$fitted.values, data_train$average_claim_size, 
     xlab = "Fitted Values", ylab = "Observed trainingValues", 
     main = "Observed vs Fitted Values")
abline(a = 0, b = 1, col = "red") # expect variance to increase as value increases for gamma
# deviance, dispersion.
dispersion <- sum(residuals(m7, type = "deviance")^2) / m7$df.residual
cat("Dispersion:", dispersion, "\n") # should be close to 1 for gamma

# Plot squared residuals vs fitted values
# Get fitted values and residuals
fitted_values <- m7$fitted.values
residuals <- residuals(m7, type = "response")
plot(fitted_values, residuals^2, 
     xlab = "Fitted Values (Mean)", 
     ylab = "Squared Residuals (Variance)", 
     main = "Variance vs. Mean in Gamma GLM",
     ylim = c(-100, 1000000 ))
abline(lm(residuals^2 ~ fitted_values), col = "red")  # Add trend line


## lognormal
m1 <- glm(log(average_claim_size) ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + quote_time_group, 
          family = "gaussian", 
          weights = claimNb, 
          data = data_train)
summary(m1)
par(mfrow = c(2, 2))
plot(m1)



plot(m1$fitted.values, data_train$average_claim_size, 
     xlab = "Fitted Values", ylab = "Observed trainingValues", 
     main = "Observed vs Fitted Values")
abline(a = 0, b = 1, col = "red") # expect variance to ____ as value increases...

#dispersion <- sum(residuals(m1, type = "deviance")^2) / m1$df.residual
#cat("Dispersion:", dispersion, "\n") # should be close to 1