library(tidyverse)
library(MLmetrics) #rmse
library(statmod) # AIC
glm_performance <- function(model, test_data= data_test) {
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
merged <- read_csv("Data/mergedclaims.csv")

#merged$pet_de_sexed_age <- as.factor(merged$pet_de_sexed_age)
#merged %>% mutate(across(where(is.character), \(x) type.convert(x, as.is = TRUE)))
merged <- merged %>%mutate(across(
  c(pet_gender, pet_de_sexed_age, nb_address_type_adj, nb_suburb, nb_postcode,
    nb_state, pet_age_years, nb_breed_type, nb_breed_trait),
  as.factor
  ))
merged <- merged %>% # a variable if a breed has high average claim size. Analysed in data exploration below.
  mutate(severe_breed = ifelse(breed %in% c("american staffordshire terrier", "groodle", "pomeranian", "spoodle"), breed, "not_severe")) %>%
  mutate(severe_breed = as.factor(severe_breed))

# manipulating protected land to get a number per lga.
protectedland <- read_csv("Data/external/protectedlandlga.csv")
t <- protectedland %>% select(-Year) %>% 
  group_by(Label) %>%
  summarize(across(where(is.numeric), \(x) max(x, na.rm = TRUE))) %>%
  mutate(protectedLand = `Total protected land area (ha)`) %>%
  select(Label, protectedLand)
t <- t %>% filter(protectedLand != 0)
by <- join_by(nb_suburb == Label)
# new dataset to use.
merged2 <- left_join(merged, t, by)
merged2 <- merged2 %>% mutate(protectedLand = ifelse(is.na(protectedLand), 0, protectedLand))

# Modelling ####
set.seed(888)  
train_indices <- sample(1:nrow(merged2), size = 0.7 * nrow(merged2)) 
data_train = merged2[train_indices, ]
data_test = merged2[-train_indices, ]
model1 <- glm(total_claim_amount ~ ., 
            data = data_train %>% select(c("total_claim_amount", "nb_breed_trait", "pet_de_sexed_age")),   family = Gamma(link = "log"))
glm_performance(model1) #rmse 914, 30913 aic gamdev : 2042.96739583235"
model2 <- glm(total_claim_amount ~ ., 
            data = data_train %>% select(c("total_claim_amount", "nb_breed_trait", "pet_de_sexed_age", "pet_age_years")),   family = Gamma(link = "log"))
glm_performance(model2) # 916 rmse, aic 30890 gamdev:2006.90559713811"
model3 <- glm(total_claim_amount ~ ., 
            data = data_train %>% select(c("total_claim_amount", "nb_breed_trait", "pet_de_sexed_age", "pet_age_years", "severe_breed")),   family = Gamma(link = "log"))
glm_performance(model3) # 953 rmse, aic 30787. gamdev 2092.64810023187"
#finally adding Total land.
model4 <- glm(total_claim_amount ~ ., 
            data = merged2 %>% select(c("total_claim_amount", "pet_de_sexed_age", "pet_age_years", "severe_breed", "protectedLand")),   family = Gamma(link = "log"))
glm_performance(model4) # unable to make protectedLand work. Maybe because so many zeros.


intersect(as.factor(protectedland$Label), merged$nb_suburb)
levels(merged$nb_suburb)
