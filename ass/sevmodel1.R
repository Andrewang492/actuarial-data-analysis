library(tidyverse)
library(ggplot2)
library(lubridate)
library(MASS) #stepAIC and glm.nb()

claims_data %>% group_by(claim_id, exposure_id) %>% summarise(claim_paid = sum(claim_paid), total_claim_amount = sum(total_claim_amount)) %>% group_by(exposure_id)  %>% summarise(claim_paid = mean(claim_paid), total_claim_amount = sum(total_claim_amount))  %>% filter(total_claim_amount > 0 & claim_paid > 0)

#### Data setup ####
claims_data <- read_csv("Data/UNSW_claims_data.csv")

#Skipping first column which is just numbers
earned_data <- read_csv("UNSW_earned_data_adjusted_Sep27.csv")[, -1]

# price_output <- read_csv("Data/Sample_price_output_file.csv")

# Changing claim status and condition category to factors
claims_data <- claims_data %>%
  mutate(
    claim_status = as.factor(claim_status),
    condition_category = as.factor(condition_category)
  )
#Changing variables to factors
earned_data <- earned_data %>%
  mutate(across(
    c(pet_gender, pet_de_sexed_age, nb_address_type_adj, nb_suburb, nb_postcode,
      nb_state, pet_age_years, nb_breed_type, nb_breed_trait),
    as.factor
  ))
#Number of claims per exposure id, need to divide this by the exposure
exposure_id_claims_count <- claims_data %>%
  group_by(exposure_id) %>%
  summarize(unique_claim_count = n_distinct(claim_id))
#Cost per exposure. Would need to do offsetting by claimnb or something, since that impacts an exposure's claim cost.
exposure_id_claims_cost <- claims_data %>%
  group_by(exposure_id) %>%
  summarize(sum_claims_cost = sum(total_claim_amount)) # kinda wanted to sum costs for a single claim, then costs for an exposure, but done in one sum.

# For severity, might be better just to look at claims rather than each exposure.
claim_costs <- claims_data %>% group_by(claim_id, exposure_id) %>% summarise(sum_claims_cost = sum(total_claim_amount))

# Condense data to talk about each exposure. !!! Grouping OK????
exposures <- earned_data %>% group_by(exposure_id) %>% summarise(max_tenure = max(tenure), first_uw_date = min(UW_Date), total_earned_units = sum(earned_units), across(-c(UW_Date, tenure, earned_units), first))
sev_data1 <- merge(claim_costs, exposures, by = "exposure_id")
sev_data <- sev_data1 %>% filter(sum_claims_cost != 0)
rm(sev_data1)# 57 entries have 0 claim cost:
  #check inception and uw first are different.
# 500 policies where first uw month is not the same as inception date. 
# sev_data %>% select(nb_policy_first_inception_date, first_uw_date) %>% filter(month(nb_policy_first_inception_date) != month(first_uw_date))

#### Doing Severity ####
set.seed(888)  
train_indices <- sample(1:nrow(claim_costs), size = 0.7 * nrow(claim_costs)) 
sev_data_train = sev_data[train_indices, ]
# Create the real density plot using ggplot
ggplot(claim_costs, aes(x = sum_claims_cost)) +
  geom_density(color = "darkblue") +
  labs(title = "Empirical Density of Claim Amounts",
       x = "Claim Total",
       y = "Density") +
#  xlim(0, 10000) + 
  theme_minimal()
summary(claim_costs)

# build gamma and inverse gaussian with log link.
sevGamma_full <- glm(sum_claims_cost ~ pet_gender + pet_de_sexed + pet_age_months +
                       nb_contribution + nb_excess + nb_address_type_adj + nb_state + nb_contribution_excess +
                       owner_age_years + nb_number_of_breeds + nb_average_breed_size +  nb_breed_type + nb_breed_trait + is_multi_pet_plan ,
                     family = Gamma(link = "log"), 
                     data = sev_data_train)
summary(sevGamma_full) #ass
# Subset selection
sev_none <- glm(sum_claims_cost ~ 1,
                family = Gamma(link = "log"), 
                data = sev_data_train)
sevGamma_back<-stepAIC(sevGamma_full,
                  direction = "backward",
                  k = 2,
                  scope = list(upper = sevGamma_full, lower = sev_none)
)
# Randomised quantile Residuals (difficult)
# qq plot (on residuals)
#chisq. Not appropriate?
# F Test
# # RMSE
# model Deviance
# Gamma deviance for each observation
# AIC
# Spearman correlation

# Deciles test (very difficult)
