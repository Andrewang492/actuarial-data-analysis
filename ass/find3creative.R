library(tidyverse)
library(MLmetrics) #rmse
library(statmod) # AIC


merged <- read_csv("Data/mergedclaims.csv")
merged$pet_de_sexed_age <- as.factor(merged$pet_de_sexed_age)
#summary(merged$pet_de_sexed_age)
# Helper functions ####
glm_performance <- function(model, test_data= data_test) {
  predictions <- predict.glm(model, newdata = test_data, type="response")
  print(anova(model))
  print(paste("RMSE: ", RMSE(predictions, test_data$total_claim_amount)))
  print(paste("AIC: ", AIC(predictions, test_data$total_claim_amount)))
  print(AIC(model))
  ggplot() +
    geom_density(data = data.frame(predictions = predictions),
                 mapping = aes(x=predictions)) +
    labs(title = "f") +
    geom_density(data = test_data, mapping = aes(x=total_claim_amount), col = 'red') +
    xlim(0,7500) # Note that we cut observations.
}
# exploration on old merged data####
#merged %>% select(claim_start_date, nb_policy_first_inception_date, UW_Date, quote_date)
#merged %>% select(claim_id, claim_start_date, nb_policy_first_inception_date, UW_Date, quote_date) %>%
#  filter(claim_start_date < nb_policy_first_inception_date) # one claim made claim before getting, invalid times, to remove.
# claim_c5f5cbc0-563f-42cd-851f-d9783ebd6442
# manipulation on new data ####
dwellingstock <- read_csv("Data/external/dwellingstock.csv")

dwellingstock <- dwellingstock %>% filter(Year == 2022) %>%
  mutate(state = Label) %>%
  mutate(state = ifelse(Label == "New South Wales", "NSW", state)) %>%
  mutate(state = ifelse(Label == "Victoria", "VIC", state)) %>%
  mutate(state = ifelse(Label == "South Australia", "SA", state)) %>%
  mutate(state = ifelse(Label == "Western Australia", "WA", state)) %>%
  mutate(state = ifelse(Label == "Tasmania", "TAS", state)) %>%
  mutate(state = ifelse(Label == "Northern Territory", "NT", state)) %>%
  mutate(state = ifelse(Label == "Australian Capital Territory", "ACT", state)) %>%
  mutate(state = ifelse(Label == "Queensland", "QLD", state)) %>%
  filter(state != "Australia" & state != "Other Territories") %>%
  dplyr::select(-Code, -Label, -Year)

merged <- merge(merged, dwellingstock,  by.x = "nb_state", by.y = "state")
dwellingvars <- colnames(dwellingstock)[4:length(colnames(dwellingstock))-1] #not label, year, state, code.

# Modelling ####
set.seed(888)  
train_indices <- sample(1:nrow(merged), size = 0.7 * nrow(merged)) 
data_train = merged[train_indices, ]
data_test = merged[-train_indices, ]

dwellingmodel <- glm(total_claim_amount ~ ., 
                       data = data_train %>% dplyr::select(c("total_claim_amount", dwellingvars)),family = Gamma(link = "log"))
glm_performance(dwellingmodel)
predictions <- predict.glm(dwellingmodel, newdata = data_test, type="response")

