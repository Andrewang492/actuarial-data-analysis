library(MASS) #stepAIC and glm.nb()
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

# remove the ones without a claim amount
merged <- merged %>% filter(average_claim_size > 0) %>%
  group_by(nb_breed_trait) %>%
  filter(n() > 5) %>%
  ungroup()
#### Doing Severity ####
set.seed(888)  
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 
data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]

#Stepwise selection again.
sevGamma_full <- glm(average_claim_size ~ pet_gender + pet_de_sexed_age + pet_is_switcher + pet_age_months +
                       nb_address_type_adj + nb_state + nb_contribution_excess +
                       owner_age_years + nb_number_of_breeds + nb_average_breed_size +  nb_breed_type + nb_breed_trait + is_multi_pet_plan +
                       quote_date,
                     family = Gamma(link = "log"),
                     weights = claimNb,
                     data = data_train)
sev_none <- glm(average_claim_size ~ 1,
                family = Gamma(link = "log"), 
                weights = claimNb,
                data = data_train)
aic_back<-stepAIC(sevGamma_full,
                       direction = "backward",
                       k = 2,
                       scope = list(upper = sevGamma_full, lower = sev_none)
)
#pet_gender + pet_de_sexed_age + pet_is_switcher + 
#  pet_age_months + nb_address_type_adj + nb_state + nb_contribution_excess + 
#  owner_age_years + nb_number_of_breeds + nb_average_breed_size + 
#  nb_breed_type + is_multi_pet_plan + quote_date
bic_back<-stepAIC(sevGamma_full,
                       direction = "backward",
                       k = log(nrow(data_train)),
                       scope = list(upper = sevGamma_full, lower = sev_none)
)
#average_claim_size ~ nb_contribution_excess + nb_average_breed_size


  
bic_forward<-stepAIC(sev_none,
                       direction = "forward",
                       k = log(nrow(data_train)),
                       scope = list(upper = sevGamma_full, lower = sev_none)
)
#average_claim_size ~ nb_contribution_excess
bic_forward

  
