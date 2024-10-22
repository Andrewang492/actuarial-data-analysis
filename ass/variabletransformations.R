library(tidyverse)
library(MLmetrics) #rmse
library(statmod) # AIC
# THIS ONE DOES AVERAGE CLAIM SIZE
glm_performance <- function(model, test_data= data_test) {
  #model$xlevels[["nb_breed_trait"]] <- union(model$xlevels[["nb_breed_trait"]], levels(data_test$nb_breed_trait))
  predictions <- predict.glm(model, newdata = test_data, type="response")
  expected <- test_data$average_claim_size
  print(anova(model))
  print(summary(model))
  print(paste("RMSE: ", RMSE(predictions, test_data$average_claim_size)))
  print(paste("AIC: ", AIC(model)))
  print(paste("Model deviance: ", deviance(model)))
  print(paste("test gamma deviance: ", 2 * sum((predictions - expected) / expected - log(predictions / expected))))
  ggplot() +
    geom_density(data = data.frame(predictions = predictions),
                 mapping = aes(x=predictions)) +
    labs(title = "f") +
    geom_density(data = test_data, mapping = aes(x=average_claim_size), col = 'red')
}


gammadeviance <- function(model, test_data = data_test) {
  predictions <- predict.glm(model, newdata = test_data, type="response")
  null_model <- glm(y ~ 1, family = Gamma(link = "log"), data = data)
}

# merged data manip ####
merged <- read_csv("Data/merged.csv")
# turn into factors
merged <- merged %>%mutate(across(
  c(pet_gender, pet_de_sexed_age, nb_address_type_adj, nb_suburb, nb_postcode,
    nb_state, pet_age_years, nb_breed_type, nb_breed_trait),
  as.factor
))

# age squared

# exploration ####
ggplot(merged %>% group_by(pet_age_months) %>% mutate(average_claim_size = mean(average_claim_size)), aes(x = pet_age_months, y = average_claim_size)) +
  geom_line(color = "blue", size = 3) +  # Points with size and color
  labs(
    title = "Scatterplot: Average Claim Size vs contiuous variable",
    x = "continuous variable",
    y = "Average Claim Size ($)"
  ) +
  theme_minimal()

# Modelling ####
# remove the ones without a claim amount
merged <- merged %>% filter(average_claim_size > 0) %>%
  group_by(nb_breed_trait) %>%
  filter(n() > 5) %>%
  ungroup()
set.seed(888)  

train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 
data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]
# turn into factors
merged <- merged %>%mutate(across(
  c(pet_gender, pet_de_sexed_age, nb_address_type_adj, nb_suburb, nb_postcode,
    nb_state, pet_age_years, nb_breed_type, nb_breed_trait),
  as.factor
))

m1 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait, 
                      family = Gamma(link = "log"), 
                      weights = claimNb, 
                      data = data_train)
glm_performance(m1)
m2 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + I(pet_age_months^2), 
                      family = Gamma(link = "log"), 
                      weights = claimNb, 
                      data = data_train)
glm_performance(m2)
m3 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + I(pet_age_months^2) + log(pet_age_months), 
                      family = Gamma(link = "log"), 
                      weights = claimNb, 
                      data = data_train)
glm_performance(m3) # 30300 2567 5022
m4 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + log(pet_age_months), 
                      family = Gamma(link = "log"), 
                      weights = claimNb, 
                      data = data_train)
glm_performance(m4) # 30300 2568 5002
m5 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + log(pet_age_months) + quote_time_group, 
                      family = Gamma(link = "log"), 
                      weights = claimNb, 
                      data = data_train)
glm_performance(m5) # 30304 2566 4993
m6 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + pet_age_months + quote_time_group, 
                      family = Gamma(link = "log"), 
                      weights = claimNb, 
                      data = data_train)
glm_performance(m6) # 30325 2587 5021


# average breed size * pet breed
c1 <- glm(average_claim_size ~ I(pet_age_months^2) + 
  pet_age_months:nb_breed_type + nb_excess + nb_address_type_adj + 
  nb_contribution_excess + owner_age_years + nb_average_breed_size * 
  pet_de_sexed_age + nb_breed_type + nb_breed_trait,
    family = Gamma(link = "log"), 
    weights = claimNb, 
    data = data_train)
glm_performance(c1) # 30265 2519 5708
