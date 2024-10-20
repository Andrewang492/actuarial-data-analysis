library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggcorrplot)
library(dplyr)
library(statmod)
library(MLmetrics)
# Helper functions ####
glm_performance <- function(model, test_data= data_test) {
  predictions <- predict.glm(model, newdata = test_data, type="response")
  print(anova(model))
  print(paste("RMSE: ", RMSE(predictions, test_data$total_claim_amount)))
  print(paste("RMSE: ", AIC(predictions, test_data$total_claim_amount)))
  print(AIC(model))
  ggplot() +
    geom_density(data = data.frame(predictions = predictions),
                 mapping = aes(x=predictions)) +
    labs(title = "f") +
    geom_density(data = test_data, mapping = aes(x=total_claim_amount), col = 'red') +
    xlim(0,7500) # Note that we cut observations.
}
# Data manipulation ####
merged <- read_csv("Data/merged.csv")
merged$pet_de_sexed_age <- as.factor(merged$pet_de_sexed_age)
condition_vars <- grep("^condition", names(sev_data_train), value = TRUE)
merged <- merged %>% mutate(breeds_list = strsplit(merged$nb_breed_name_unique_concat, ","))
merged <- merged %>% # a variable if a breed has high average claim size. Analysed in data exploration below.
  mutate(severe_breed = ifelse(breed %in% c("american staffordshire terrier", "groodle", "pomeranian", "spoodle"), breed, "not_severe"))

#### data exploration ####
merged %>% mutate(n_breeds = lapply(breeds_list, length)) %>% filter(n_breeds != nb_number_of_breeds) #!!! One entry to remove. Change to 2 nb_breeds. claim_fcf1e768-655c-48e6-8e68-54ac7092523a

#All breeds in breed set, all good
#View(merged %>% filter(nb_breed_name_unique != "cross"))

#what conditions are worst.
model <- lm(total_claim_amount ~ ., data = merged %>% select(c("total_claim_amount", condition_vars)))
anova(model) # gastrointestinal, allergies and skin, eyes and ears, ortho, Other.
model <- glm(total_claim_amount ~ ., 
             data = merged %>% select(c("total_claim_amount", condition_vars)),   family = Gamma(link = "log"))
anova(model) # Also ingestion.

#breeds
# Create density plot with overlapping lines for each ____
ggplot(merged, aes(x = total_claim_amount, color = nb_breed_type)) +
  geom_density(linewidth = 2) +  # Size adjusts the thickness of the lines
  labs(title = "Distribution of Claim Amounts by Breed",
       x = "Claim Amount",
       y = "Density")
ggplot(merged %>% group_by(breed) %>% summarise(ave_claim = mean(total_claim_amount)) %>% select(breed, ave_claim),
       aes(x = breed, y = ave_claim)) +
  geom_col()
merged %>% group_by(breed) %>% summarise(ave_claim = mean(total_claim_amount)) %>% select(breed, ave_claim) %>% 
           filter(ave_claim > mean(ave_claim) + sqrt(var(ave_claim)))
# american staffordshire terrier, groodle, pomeranian, spoodle


####Modelling####
set.seed(888)  
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 
data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]
#conditions modelling ----
selected_conditions <- c(condition_vars[1:4], "condition_Ortho", "condition_Ingestion")
condition_model <- glm(total_claim_amount ~ ., 
                       data = data_train %>% select(c("total_claim_amount", selected_conditions)),family = Gamma(link = "log"))
glm_performance(condition_model) #rmse 1057

condition_model_all <- glm(total_claim_amount ~ ., 
                       data = data_train %>% select(c("total_claim_amount", condition_vars)),family = Gamma(link = "log"))
glm_performance(condition_model_all) #rmse 1059
# breeds modelling ----
breed_model <- glm(total_claim_amount ~ ., 
                       data = data_train %>% select(c("total_claim_amount", "breed")),   family = Gamma(link = "log"))
glm_performance(breed_model) #rmse 1090
#
breed_5_model <- glm(total_claim_amount ~ ., family = Gamma(link = "log"),
                   data = data_train %>% select(c("total_claim_amount", "severe_breed"))) 
glm_performance(breed_5_model) #rmse 1081
breed_trait_model <- glm(total_claim_amount ~ ., family = Gamma(link = "log"),
                   data = data_train %>% select(c("total_claim_amount", "nb_breed_trait"))) 
glm_performance(breed_trait_model) #rmse 1073
# age modelling ----
age_model <- glm(total_claim_amount ~ ., 
                                data = data_train %>% select(c("total_claim_amount", "pet_age_months")),   family = Gamma(link = "log"))
glm_performance(age_model) #rmse 1067, no significance.

# random combinations
gam1 <- glm(total_claim_amount ~ ., 
                                data = data_train %>% select(c("total_claim_amount", "pet_age_months", selected_conditions, "severe_breed")),   family = Gamma(link = "log"))
glm_performance(gam1) #rmse 1069
gam2 <- glm(total_claim_amount ~ ., 
                                data = data_train %>% select(c("total_claim_amount", selected_conditions, "severe_breed")),   family = Gamma(link = "log"))
glm_performance(gam2) #rmse 1071
gam3 <- glm(total_claim_amount ~ ., 
                data = data_train %>% select(c("total_claim_amount", selected_conditions, "nb_breed_trait")),   family = Gamma(link = "log"))
glm_performance(gam3) # rmse 1064 aic 30582

# tweak to data set of seeed 888 so that it doesnt test where it can't on desex age 2 and 8
data_train_desex28 <- rbind(data_train, merged %>% filter(pet_de_sexed_age == 2 | pet_de_sexed_age == 8 ))
data_test_desex28 <- data_test %>% filter(pet_de_sexed_age != 2 & pet_de_sexed_age != 8)
gam4 <- glm(total_claim_amount ~ ., 
                data = data_train_desex28 %>% select(c("total_claim_amount", selected_conditions, "nb_breed_trait", "pet_de_sexed_age")),   family = Gamma(link = "log"))
glm_performance(gam4, test_data = data_test_desex28) # rmse 1061, AIC 30612


