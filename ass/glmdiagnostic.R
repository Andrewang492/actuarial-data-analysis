library(ggplot2)
library(tidyverse)
glmdiagnosis <- function(m, data = data_train) {
  print(summary(m))
  par(mfrow = c(2, 2))
  plot(m)
  # Plot observed vs. fitted values
  plot(m$fitted.values, data$average_claim_size, 
       xlab = "Fitted Values", ylab = "Observed training Values", 
       main = "Observed vs Fitted Values",
       ylim = c(min(m$fitted.values), max(m$fitted.values)))
  abline(a = 0, b = 1, col = "red") # expect variance to increase as value increases for gamma
  # deviance, dispersion.
  dispersion <- sum(residuals(m, type = "deviance")^2) / m$df.residual
  cat("Dispersion:", dispersion, "\n") # should be close to 1 for gamma
  
  # Plot squared residuals vs fitted values
  # Get fitted values and residuals
  fitted_values <- m$fitted.values
  residuals <- residuals(m, type = "response")
  plot(fitted_values, residuals^2, 
       xlab = "Fitted Values (Mean)", 
       ylab = "Squared Residuals (Variance)", 
       main = "Variance vs. Mean in GLM")
  abline(lm(residuals^2 ~ fitted_values), col = "red")  # Add trend line
}

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

# Pre modelling diagnoisis ####


ggplot(data = merged, mapping = aes(y = average_claim_size, x = wait_after_quote)) +
  geom_smooth()
ggplot(data = merged, mapping = aes(y = average_claim_size, x = quote_date)) +
  geom_smooth()
ggplot(data = merged, mapping = aes(y = log(average_claim_size), x = pet_age_months)) +
  geom_smooth() +
  geom_point()
ggplot(data = merged, mapping = aes(y = average_claim_size, x = pet_age_months)) +
  geom_smooth() +
  geom_point() +
  ylim(c(0, 2000))
ggplot(data = merged, mapping = aes(y = average_claim_size, x = owner_age_years)) +
  geom_smooth()


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

## gamma ####
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
     main = "Observed vs Fitted Values",
     ylim=c(0,1500))
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

plot(residuals(m7, type="deviance"))

## lognormal ####
m1 <- glm(log(average_claim_size) ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + quote_time_group, 
          family = "gaussian", 
          weights = claimNb, 
          data = data_train)
summary(m1)
par(mfrow = c(2, 2))
plot(m1)



plot(exp(m1$fitted.values), data_train$average_claim_size,  #note i exponentiate
     xlab = "Fitted Values", ylab = "Observed trainingValues", 
     main = "Observed vs Fitted Values",
     ylim = c(0, 1000))
abline(a = 0, b = 1, col = "red") # expect variance to be square of mean as value increases...

#dispersion <- sum(residuals(m1, type = "deviance")^2) / m1$df.residual
#cat("Dispersion:", dispersion, "\n") # should be close to 1

## weakest gamma ####
m2 <- glm(average_claim_size ~ 1, 
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m2)
## super weak gamma ####
m3 <- glm(average_claim_size ~ pet_age_months, 
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m3)
## logged response on gamma ####
logmerged <- merged %>% mutate(average_claim_size = log(average_claim_size)) %>%
    filter(average_claim_size > 0)
train_indices2 <- sample(1:nrow(logmerged), size = 0.7 * nrow(logmerged)) 
data_train_log = logmerged[train_indices2, ]
data_test_log = logmerged[-train_indices2, ]

m4 <- glm(average_claim_size ~ pet_age_months + I(pet_age_months^2) + pet_de_sexed_age + nb_breed_trait + quote_time_group, 
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train_log)
glmdiagnosis(m4, data = data_train_log)


