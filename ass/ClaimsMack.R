library(tidyverse)
library(ggplot2)

claims_data <- read_csv("Data/UNSW_claims_data.csv")

#Skipping first column which is just numbers
earned_data <- read_csv("Data/UNSW_earned_data_adjusted_Sep27.csv")[, -1]

# price_output <- read_csv("Data/Sample_price_output_file.csv")

# Changing claim status and condition category to factors
claims_data <- claims_data %>%
  mutate(
    claim_status = as.factor(claim_status),
    condition_category = as.factor(condition_category)
  )
summary(claims_data)
#Changing variables to factors
earned_data <- earned_data %>%
  mutate(across(
    c(pet_gender, pet_de_sexed_age, nb_address_type_adj, nb_suburb, nb_postcode,
      nb_state, pet_age_years, nb_breed_type, nb_breed_trait),
    as.factor
  ))

#Checking to see if the exposure id and exposure id 1s are the same
unequal_rows <- earned_data %>%
  filter(exposure_id != exposure_id_1) %>%
  nrow()


# Check pet_age_years and pet_age_months match



summary(earned_data)

#Number of claims per exposure id, need to divide this by the exposure
exposure_id_claims_count <- claims_data %>%
  group_by(exposure_id) %>%
  summarize(unique_claim_count = n_distinct(claim_id))

n_distinct(earned_data$exposure_id) #9657 distinct exposure ids 

#Filter the earned data to reduce to unique exposure ids with the maximum tenure
# values, remove negative tenure values, add up the earned units to get the exposure
clean_earned_data <- earned_data %>%
  filter(tenure >= 0) %>%
  group_by(exposure_id) %>%
  summarise(
    max_tenure = max(tenure),
    total_earned_units = sum(earned_units),
    across(
      !c(UW_Date, tenure, nb_policy_first_inception_date, lead_date_day, quote_date, quote_time_group, exposure_id_1, row_num, earned_units), 
      ~ first(.), 
      .names = "{col}"
    )
  )
# Merging the cleaned earned data with the claims count based on exposure id, if there are no claims made then claims count is 0
# Frequency is computed as
freq <- clean_earned_data %>%
  left_join(exposure_id_claims_count, by = "exposure_id") %>%
  mutate(
    unique_claim_count = replace_na(unique_claim_count, 0), # Replace NAs with 0
    frequency = unique_claim_count / total_earned_units     # Create frequency column
  ) %>%
  mutate(
    frequency = ifelse(is.nan(frequency), 0, frequency) # Replace NaN values with 0
  )

###################################### EDA ##################################### 
ggplot(freq, aes(x = frequency)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Claims Frequency", x = "Claims Frequency", y = "Density") +
  xlim(0, quantile(freq$frequency, 0.95)) +
  theme_minimal()

#breed trait and claim frequency
breed_trait_and_freq <- freq %>%
  group_by(nb_breed_trait) %>%
  summarise(total_claims = sum(unique_claim_count, na.rm = TRUE))

ggplot(breed_trait_and_freq, aes(x = nb_breed_trait, y = total_claims)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Claim Counts by Breed Trait", x = "Breed Trait", y = "Total Claim Counts") 


# suburb and claim frequency
# Thinking of not using postcode as it can cover large areas with multiple suburbs
suburb_and_freq <- freq %>%
  group_by(nb_suburb) %>%
  summarise(total_claims = sum(unique_claim_count, na.rm = TRUE)) %>%
  filter(total_claims > 0)

# Claim type
ggplot(claims_data, aes(x = condition_category, y = total_claim_amount)) +
  geom_boxplot(fill = "skyblue", color = "black", outlier.color = "red", outlier.shape = 16) +
  labs(title = "Distribution of Total Claim Amounts by Condition Category",
       x = "Condition Category", 
       y = "Total Claim Amount") +


####################################### Model ##################################
set.seed(88)
train_indices <- sample(1:nrow(freq), size = 0.7 * nrow(freq))

#Exclude the exposure_id

#Note: for offset with log(total earned units) need to remove the 0s in total earned units
# freqPoisson <- glm(unique_claim_count ~ pet_gender + 
#                       pet_de_sexed + nb_state + nb_breed_type + nb_breed_trait,
#                     data = freq, subset = train_indices,
#                     offset = log(total_earned_units), family = poisson(link="log"))

# Didn't use pet_age_years as pet_age_months was already used
freqPoisson_full <- glm(unique_claim_count ~ pet_gender + pet_de_sexed + pet_age_months +
                    nb_contribution + nb_excess + nb_address_type_adj + nb_state + nb_contribution_excess +
                    owner_age_years + nb_number_of_breeds + nb_average_breed_size +  nb_breed_type + nb_breed_trait + is_multi_pet_plan ,
                    data = freq, subset = train_indices,
                    family = poisson(link="log"))
summary(freqPoisson_full)

#Note: Refitting model with variables in diff order for ANOVA and multi-level categorical variables W5 Lab Soln p.8
anova(freqPoisson_full,test="Chisq")
