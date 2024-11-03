# Didn't end up following through with this file.####
library(tidyverse)

claims_data <- read_csv("Data/UNSW_claims_data.csv")  
earned_data <- read_csv("Data/UNSW_earned_data_adjusted_Sep27.csv")[,-1]

# Claims data first ####
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
      nb_state, pet_age_years, nb_breed_type, nb_breed_name_unique, nb_breed_trait),
    as.factor
  ))

# Delete duplicate rows
claims_data %>% nrow() # 3487
claims_data %>% distinct() %>% nrow() # 3474
claims_data <- claims_data %>% distinct()

# There can be multiple rows with the same claim ID
# This occurs when the claim involves more than one condition category (Ingestion and Eyes and Ears etc)
# 465 claim IDs occuring more than once
claims_data %>%
  group_by(claim_id) %>%          
  filter(n() > 1) %>%
  arrange(claim_id)

# But (claim_id, condition_category)is also not unique
# 58 rows with the same (claim_id, condition_category)
claims_data %>%
  group_by(claim_id, condition_category) %>% 
  filter(n() > 1) %>%
  ungroup()

claims_data <- claims_data %>% filter(total_claim_amount > 10^-3 & claim_paid > 10^-3) #note that 0 claim paids with say 200 total_claims are removed too... Remove 395. Could remove only 139

claims_data <- claims_data %>%
  group_by(across(-c(claim_paid, total_claim_amount))) %>%
  summarise(claim_paid = sum(claim_paid),
            total_claim_amount = sum(total_claim_amount))

claimNbByExposure <- claims_data %>% group_by(exposure_id) %>% summarise(claimNb = n_distinct(claim_id))
claims_data <- claims_data %>% left_join(claimNbByExposure, by="exposure_id")
claims_data$claimNb <- ifelse(is.na(claims_data$claimNb), 0, claims_data$claimNb)
