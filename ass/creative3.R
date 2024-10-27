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

glm_performance2 <- function(model, test_data= data_test) {
  #model$xlevels[["nb_breed_trait"]] <- union(model$xlevels[["nb_breed_trait"]], levels(data_test$nb_breed_trait))
  predictions <- predict.glm(model, newdata = test_data, type="response")
  
  expected <- test_data$total_claim_amount
  print(anova(model))
  print(paste("RMSE: ", RMSE(predictions, test_data$total_claim_amount)))
  #print(paste("AIC: ", AIC(predictions, test_data$total_claim_amount)))
  print(paste("AIC: ", AIC(model)))
  print(paste("Model deviance: ", deviance(model)))
  print(paste("test gamma deviance: ", 2 * sum((predictions - expected) / expected - log(predictions / expected))))
  ggplot() +
    geom_density(data = data.frame(predictions = predictions),
                 mapping = aes(x=predictions)) +
    labs(title = "f") +
    geom_density(data = test_data, mapping = aes(x=total_claim_amount), col = 'red') +
    xlim(0,7500) # Note that we cut observations.
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
# qi indicator
qiind <- read_csv("Data/External/qiindicators.csv")
qiind$QI_INDICATOR <- as.factor(qiind$QI_INDICATOR)
sort(intersect(as.factor(qiind$SA2_NAME_2011), merged$nb_suburb))
#levels(merged$nb_suburb)
b <- join_by(nb_suburb == SA2_NAME_2011)
merged <- merged %>% left_join(qiind, b)

# quote_date
merged %>% mutate(wait_after_quote = nb_policy_first_inception_date - quote_date) %>% 
  select(exposure_id, wait_after_quote, nb_policy_first_inception_date, quote_date) %>% 
  filter((wait_after_quote) < -2880)
# only one quote that is like super wrong.
# Make a wait_after_quote variable.
merged <- merged %>% mutate(wait_after_quote = nb_policy_first_inception_date - quote_date) %>% 
  mutate(wait_after_quote = ifelse(wait_after_quote < 0, 0, wait_after_quote))


#merged %>% select(exposure_id) %>% filter(quote_date > nb_policy_first_inception_date)
# Modelling ####
set.seed(888)  
# remove the ones without a claim amount
merged <- merged %>% filter(average_claim_size > 0) %>%
  group_by(nb_breed_trait) %>%
  filter(n() > 5) %>%
  ungroup()
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 

data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]
sevGamma_full <- glm(total_claim_amount ~ offset(log(claimNb)) + pet_de_sexed_age + pet_age_years + nb_breed_trait, 
                     family = Gamma(link = "log"), 
                     weights = claimNb, 
                     data = data_train)
glm_performance2(sevGamma_full)

sevGamma <- glm(total_claim_amount ~ offset(log(claimNb)) + nb_breed_trait, 
                     family = Gamma(link = "log"), 
                     weights = claimNb, 
                     data = data_train)
glm_performance2(sevGamma)


# could pet age log and square. 

# USING AVERAGE CLAIM SIZE ####
# use qi indicator
m1 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait, 
                     family = Gamma(link = "log"), 
                     weights = claimNb, 
                     data = data_train)
glm_performance(m1)

m2 <- glm(average_claim_size ~ nb_breed_trait, 
                family = Gamma(link = "log"), 
                weights = claimNb, 
                data = data_train)
glm_performance(m2)


m3 <- glm(average_claim_size ~ QI_INDICATOR + pet_de_sexed_age + pet_age_years + nb_breed_trait, 
                family = Gamma(link = "log"), 
                weights = claimNb, 
                data = data_train)
glm_performance(m3) # note that it removes a lot of things.

# LOOKING AT CONTINUOUS VARIABLES TO TRANSFORM

#ggplot(merged %>% group_by(owner_age_years) %>% mutate(average_claim_size = mean(average_claim_size)), aes(x = owner_age_years, y = average_claim_size)) +
#  geom_line(color = "blue", size = 3) +  # Points with size and color
#  labs(
#    title = "Scatterplot: Average Claim Size vs contiuous variable",
#    x = "continuous (years)",
#    y = "Average Claim Size ($)"
#  ) +
#  theme_minimal()

m2a <- glm(average_claim_size ~ pet_age_years +pet_de_sexed_age + nb_breed_trait, 
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glm_performance(m2a)
summary(m2a)

m4 <- glm(average_claim_size ~ pet_age_years +pet_de_sexed_age + nb_breed_trait + I(owner_age_years^2) , 
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glm_performance(m4) #4994
summary(m4)

m5 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + I(log(owner_age_years)), 
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glm_performance(m5)

# quote time
m6 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + wait_after_quote, 
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glm_performance(m6) # bad, test gamma 5017

m7 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + quote_time_group, 
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glm_performance(m7) # 4984
summary(m7)
