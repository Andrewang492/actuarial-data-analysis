library(ggplot2)
library(tidyverse)
library(MLmetrics) #rmse
library(statmod) # AIC

glmdiagnosis <- function(m, test_data = data_test) {
  par(mfrow=c(2,2))
  d.residuals <- residuals(m, type="deviance")
  plot(d.residuals)
  abline(a = 0, b = 0, col = "red")
  hist(d.residuals, breaks = 100)
  
  # Create a Q-Q plot of the deviance residuals
  qqnorm(d.residuals, main = "Q-Q Plot of Deviance Residuals")
  qqline(d.residuals, col = "red", lwd = 2)  # Add a reference line
  
  par(mfrow=c(1,1))
  predictions <- predict.glm(m, newdata = test_data, type="response")
  expected <- test_data$average_claim_size
  print(anova(m))
  print(summary(m))
  print(data.frame(row.names = c("RMSE", "AIC", "BIC", "Model Dev"), result = c(
    RMSE(predictions, test_data$average_claim_size),
    AIC(m),
    BIC(m),
    deviance(m)
  )))
  ggplot() +
    geom_density(data = data.frame(predictions = predictions),
                 mapping = aes(x=predictions)) +
    labs(title = "f") +
    geom_density(data = test_data, mapping = aes(x=average_claim_size), col = 'red')
}
lndiagnosis <- function(m, test_data = data_test) {
  par(mfrow=c(2,2))
  d.residuals <- residuals(m, type="deviance")
  plot(d.residuals)
  abline(a = 0, b = 0, col = "red")
  hist(d.residuals, breaks = 100)
  
  # Create a Q-Q plot of the deviance residuals
  qqnorm(d.residuals, main = "Q-Q Plot of Deviance Residuals")
  qqline(d.residuals, col = "red", lwd = 2)  # Add a reference line
  
  par(mfrow=c(1,1))
  predictions <- exp(predict.glm(m, newdata = test_data, type="response"))
  expected <- test_data$average_claim_size
  print(anova(m))
  print(summary(m))
  print(data.frame(row.names = c("RMSE", "AIC", "BIC", "Model Dev"), result = c(
    RMSE(predictions, test_data$average_claim_size),
    AIC(m),
    BIC(m),
    deviance(m)
  )))
  ggplot() +
    geom_density(data = data.frame(predictions = predictions),
                 mapping = aes(x=predictions)) +
    labs(title = "f") +
    geom_density(data = test_data, mapping = aes(x=average_claim_size), col = 'red')
}

gammadiagnosis <- function(m, test_data = data_test) {
  predictions <- predict.glm(m, newdata = test_data, type="response")
  actual <- test_data$average_claim_size
  d.residuals <- 2 * ( log(predictions/actual) + actual/predictions - 1) * sign(actual-predictions)
  print(paste("test gamma deviance: ", 2 * sum(    (actual-predictions) / predictions - log(actual / predictions)   )))
  print(paste("test gamma deviance: ", sum((d.residuals))))
  par(mfrow = c(2,2))
  plot(d.residuals, ylim= c(-4,7)) # Fails to reproduce the plot that base r can do.
  abline(a = 0, b = 0, col = "red") 
  hist(d.residuals, breaks = 100)
  par(mfrow = c(1,1))
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

#hist(merged$average_claim_size, breaks = 100)
#plot(density(merged$average_claim_size, breaks = 100))



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
set.seed(1)  
# remove the ones without a claim amount
merged <- merged %>% filter(average_claim_size > 0) %>%
  group_by(nb_breed_trait) %>%
  filter(n() > 5) %>%
  ungroup()
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 

data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]
## GLM Gamma####
m7 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + quote_time_group, 
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m7)
#result
#RMSE        507.2797
#AIC       31298.3167
#BIC       31482.8197
#Model Dev  2634.0587
## Lognormal glm ####
m1 <- glm(log(average_claim_size) ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + quote_time_group, 
          family = "gaussian", 
          weights = claimNb, 
          data = data_train)
lndiagnosis(m1, test_data=data_test)

## inverse gaussian glm (doesn't work..) ####
d2 <- data_train %>% filter(average_claim_size < 5000) 
m2 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_years + nb_breed_trait + quote_time_group, 
          family = inverse.gaussian(link = "log"),
          weights = claimNb, 
          data = d2)
glmdiagnosis(m2)

## Gamma2####
m3 <- glm(average_claim_size ~ pet_de_sexed_age + pet_age_months + owner_age_years +
            pet_age_months*owner_age_years + nb_breed_trait + quote_time_group +
            nb_contribution + pet_is_switcher, 
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m3)
gammadiagnosis(m3, test_data = data_train)
#result
#RMSE        808.6117
#AIC       30986.7447
#BIC       31151.3014
#Model Dev  2569.3024

## Jayden Gamma ####
m4 <- glm(average_claim_size ~ pet_gender + pet_de_sexed + pet_de_sexed_age  +  
  pet_is_switcher + pet_age_months + nb_contribution + nb_excess + nb_address_type_adj +
  nb_contribution_excess + owner_age_years + nb_number_of_breeds + nb_average_breed_size +
  is_multi_pet_plan + breed_group,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m4)
#result
#RMSE        809.7606
#AIC       30927.4042
#BIC       31042.0952
#Model Dev  2530.9046

## Gamma overfit jayden ####
m5 <- glm(average_claim_size ~ pet_gender + pet_de_sexed + pet_de_sexed_age  +  
  pet_is_switcher + pet_age_months + nb_contribution + nb_excess + nb_address_type_adj +
  nb_contribution_excess + owner_age_years + nb_number_of_breeds + nb_average_breed_size +
  is_multi_pet_plan  + breed_group + pet_age_months*owner_age_years +
  wait_after_quote + I(owner_age_years ^2) + I(log(owner_age_years)),
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m5)
#result
#RMSE        519.8715
#AIC       31219.9378
#BIC       31354.5751
#Model Dev  2576.3677

##down some vars. ####
m6 <- glm(average_claim_size ~ pet_de_sexed_age  +  
  pet_is_switcher + pet_age_months + nb_contribution + nb_excess +
  owner_age_years + nb_number_of_breeds + nb_average_breed_size +
  is_multi_pet_plan + breed_group + pet_age_months*owner_age_years +
  wait_after_quote + I(owner_age_years ^2) + I(log(owner_age_years)),
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m6)
#result
#RMSE        505.1081
#AIC       31231.1352
#BIC       31350.8128
#Model Dev  2593.2467

## add an interaction ####
m8 <- glm(average_claim_size ~ pet_de_sexed_age  +  
  pet_is_switcher + pet_age_months + nb_contribution + nb_excess +
  owner_age_years + nb_number_of_breeds + nb_average_breed_size +
  is_multi_pet_plan + breed_group + pet_age_months*owner_age_years +
  wait_after_quote + wait_after_quote * pet_age_months + I(owner_age_years ^2) + I(log(owner_age_years)),
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m8)
#result
#RMSE        504.2606
#AIC       31230.4780
#BIC       31355.1421
#Model Dev  2590.6324

## drop a heap  ####
m9 <- glm(average_claim_size ~ pet_de_sexed_age  +  
  pet_is_switcher + pet_age_months + nb_contribution + nb_excess + nb_number_of_breeds +
  breed_group + pet_age_months*owner_age_years +
  wait_after_quote * pet_age_months + I(owner_age_years ^2),
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(m9)
#result
#RMSE        611.3707
#AIC       30863.4200
#BIC       30978.1111
#Model Dev  2556.1989

## drop every insig.  ####
n1 <- glm(average_claim_size ~ pet_de_sexed_age + nb_contribution + nb_excess +
  breed_group,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(n1)
#result
#RMSE        498.0466
#AIC       31253.4579
#BIC       31318.2833
#Model Dev  2637.1946

## find some interaction between pet age and owner age.  ####
n2 <- glm(average_claim_size ~ pet_age_months + I(pet_age_months^2) + owner_age_years + 
  pet_age_months*owner_age_years +
  I(pet_age_months^2) * owner_age_years + pet_age_months * I(owner_age_years^2 ) +
  I(log(pet_age_months))*owner_age_years,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(n2)
# too bad
## find some interaction between pet age and owner age by adding good vairables  ####
n3 <- glm(average_claim_size ~ pet_age_months + I(pet_age_months^2) + owner_age_years + 
  pet_age_months*owner_age_years + I(pet_age_months^2) * owner_age_years + 
  pet_age_months * I(owner_age_years^2 ) + I(log(pet_age_months))*owner_age_years +
  pet_de_sexed_age + nb_contribution + nb_excess +breed_group,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(n3)
# none of the interactions really worked.
#result
#RMSE        502.5267
#AIC       31237.6262
#BIC       31347.3306
#Model Dev  2603.5908

## find some interaction between pet age and something else  ####
n4 <- glm(average_claim_size ~ pet_age_months * nb_breed_trait +
  pet_age_months * breed + pet_age_months * breed_group,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(n4)
# gives nonsense.
## find some interaction between pet age and something else  ####
n51 <- glm(average_claim_size ~ pet_age_months * nb_breed_trait +
          pet_age_months * nb_breed_type,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(n51)
#result
#RMSE        553.7586
#AIC       30307.3528
#BIC       30481.8826
#Model Dev  2575.4970
#
## explore breed infos. ####
n5 <- glm(average_claim_size ~ pet_age_months + pet_de_sexed_age + nb_contribution +
        nb_excess + breed_group + nb_breed_type,
          family = Gamma(link = "log"), 
          weights = claimNb, 
          data = data_train)
glmdiagnosis(n5)
#result
#RMSE        498.2845
#AIC       31247.8900
#BIC       31332.6616
#Model Dev  2623.6729
## n5 no weight. Halved in sample measures??? ####
n6 <- glm(average_claim_size ~ pet_age_months + pet_de_sexed_age + nb_contribution +
        nb_excess + breed_group + nb_breed_type,
          family = Gamma(link = "log"), 
          data = data_train)
glmdiagnosis(n6)
#result
#RMSE        498.2845
#AIC       31247.8900
#BIC       31332.6616
#Model Dev  2623.6729

## try with LN on n5####
l1 <- glm(log(average_claim_size) ~ pet_age_months + pet_de_sexed_age + nb_contribution +
            nb_excess + breed_group + nb_breed_type,
          family = "gaussian", 
          weights = claimNb, 
          data = data_train)
lndiagnosis(l1, test_data=data_test)
#RMSE       487.9937
#AIC       3541.7960
#BIC       3626.5677
#Model Dev 2653.3408
## LN of n1 ####

l2 <- glm(log(average_claim_size) ~ pet_de_sexed_age + nb_contribution + nb_excess +
            breed_group,
          family = "gaussian", 
          weights = claimNb, 
          data = data_train)
lndiagnosis(l2, test_data=data_test)

#result
#RMSE       488.9248
#AIC       3544.5464
#BIC       3609.3717
#Model Dev 2679.8348

## LN of Jayden m4 ####
l3 <- glm(log(average_claim_size) ~  pet_gender + pet_de_sexed + pet_de_sexed_age  +  
            pet_is_switcher + pet_age_months + nb_contribution + nb_excess + nb_address_type_adj +
            nb_contribution_excess + owner_age_years + nb_number_of_breeds + nb_average_breed_size +
            is_multi_pet_plan + breed_group,
          family = "gaussian", 
          weights = claimNb, 
          data = data_train)
lndiagnosis(l3, test_data=data_test)

#result
#RMSE       491.0028
#AIC       3536.8056
#BIC       3651.4966
#Model Dev 2612.0012

## LN of n1 no weights ####

l4 <- glm(log(average_claim_size) ~ pet_de_sexed_age + nb_contribution + nb_excess +
            breed_group,
          family = "gaussian", 
          data = data_train)
lndiagnosis(l4, test_data=data_test)

#result
#RMSE       498.7379
#AIC       3420.4388
#BIC       3485.2641
#Model Dev 1459.5430


#Favourites: m4 m9 n1 n5, l2.