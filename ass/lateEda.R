library(ggcorrplot)

merged <- read_csv("Data/merged.csv")
# Make a wait_after_quote variable.
merged <- merged %>% mutate(wait_after_quote = nb_policy_first_inception_date - quote_date) 
# turn into factors
merged <- merged %>%mutate(across(
  c(pet_gender, pet_de_sexed_age, nb_address_type_adj, nb_suburb, nb_postcode,
    nb_state, pet_age_years, nb_breed_type, nb_breed_trait, pet_is_switcher),
  as.factor
))
merged <- merged %>%mutate(
  across(c(pet_gender, pet_de_sexed, pet_de_sexed_age, pet_is_switcher, 
           nb_address_type_adj, nb_suburb, nb_postcode,
           nb_state, nb_breed_type,
           nb_breed_trait, is_multi_pet_plan, nb_breed_name_unique,
           nb_breed_name_unique_concat, quote_time_group, breed, breed_group), 
         as.factor)
)

# fix up person dob:
merged <- merged %>%
  mutate(person_dob = ifelse(is.na(person_dob), as.Date(today() - years(owner_age_years)), person_dob)) %>%
  mutate(person_dob = as.Date(person_dob))

# NOTE change dates to numerics here.
dates_to_numeric = function(d) {
  return(as.numeric(as.Date(d) - as.Date(min(d))))
}

merged <- merged %>%mutate(
  across(c(nb_policy_first_inception_date, person_dob, quote_date, UW_Date), 
         dates_to_numeric)
)
# more numerics
merged <- merged %>% mutate(wait_after_quote = as.numeric(wait_after_quote))

# Correlation matrix ####
t <- merged %>% dplyr::select(average_claim_size, nb_policy_first_inception_date, pet_age_months,
                          nb_contribution , nb_excess , nb_contribution_excess ,
                          person_dob , nb_number_of_breeds , nb_average_breed_size , 
                          lead_date_day , quote_date , UW_Date, wait_after_quote)
cor_matrix <- cor(t)
ggcorrplot(cor_matrix, lab = TRUE, type = "lower", 
           colors = c("red", "white", "blue"),
           title = "Correlation Matrix")
now() - years(28)

