library(tidyverse)
library(ggplot2)
library(lubridate)
library(MASS) #stepAIC and glm.nb()
library(ggcorrplot)
merged <- read_csv("Data/merged.csv")

merged <- merged %>% mutate(across(
  c(pet_gender, pet_de_sexed_age, nb_address_type_adj, nb_suburb, nb_postcode,
    nb_state, pet_age_years, nb_breed_type, nb_breed_name_unique, nb_breed_trait, claim_status, pet_is_switcher),
  as.factor)
)
data <- merged
## Doing Severity ####
# Create the real density plot using ggplot
ggplot(data, aes(x = total_claim_amount)) +
  geom_density(color = "darkblue") +
  labs(title = "Empirical Density of Claim Amounts",
       x = "Claim Total",
       y = "Density") +
  #  xlim(0, 10000) + 
  theme_minimal()
summary(data$total_claim_amount)

set.seed(888)  
train_indices <- sample(1:nrow(data), size = 0.7 * nrow(data)) 
sev_data_train = data[train_indices, ]

# build gamma and inverse gaussian with log link.
# remove variables that aren't really meaningful.
# Not sure about nb breed name unique, and concat.
# Nor lead_date_day.
 
condition_vars <- grep("^condition", names(sev_data_train), value = TRUE)
#pet_vars <- grep("^pet", names(sev_data_train), value = TRUE)
#nb_vars <- grep("^nb", names(sev_data_train), value = TRUE)
#selected_vars <- c(condition_vars, pet_vars, nb_vars)
#formula_str <- paste("total_claim_amount ~", paste(selected_vars, collapse = " + "))

#sevGamma_full <- glm(total_claim_amount ~ . -claim_id - claim_paid - total_claim_amount -
                   #  exposure_id - claimNb - pet_de_sexed - max_tenure - total_earned_units -
                   # pet_de_sexed - person_dob - pet_age_months - nb_breed_name_unique - 
                   #  nb_breed_name_unique_concat - lead_date_day -
                    # breed - UW_Date - quote_time_group,
                    # family = Gamma(link = "log"), 
                    # data = sev_data_train)
sevGamma_a <- glm(total_claim_amount ~ pet_de_sexed_age + pet_is_switcher + nb_address_type_adj + nb_contribution_excess,
                     family = Gamma(link = "log"), 
                     data = sev_data_train)
sevGamma_full <- glm(total_claim_amount ~ pet_gender + pet_de_sexed_age + pet_is_switcher + pet_age_months +
                       nb_contribution + nb_excess + nb_address_type_adj + nb_state + nb_contribution_excess +
                       owner_age_years + nb_number_of_breeds + nb_average_breed_size +  nb_breed_type + nb_breed_trait + is_multi_pet_plan,
                     family = Gamma(link = "log"), 
                     data = sev_data_train)
summary(sevGamma_full) #ass
sev_none <- glm(total_claim_amount ~ 1,
                family = Gamma(link = "log"), 
                data = sev_data_train)

# Subset selection

sevGamma_back<-stepAIC(sevGamma_full,
                       direction = "backward",
                       k = 2,
                       scope = list(upper = sevGamma_full, lower = sev_none)
) #breed_trait should be removed.
sevGamma_back$anova

sevGamma_forward<-stepAIC(sev_none,
                       direction = "forward",
                       k = 2,
                       scope = list(upper = sevGamma_full, lower = sev_none)
) # only keep nb_excess 
sevGamma_forward$anova

sevGamma_back_a<-stepAIC(sevGamma_a,
                       direction = "backward",
                       k = 2,
                       scope = list(upper = sevGamma_a, lower = sev_none)
) #breed_trait best off removed.
sevGamma_forward_a<-stepAIC(sev_none,
                       direction = "forward",
                       k = 2,
                       scope = list(upper = sevGamma_a, lower = sev_none)
) # only is_switcher
t <- data[c("total_claim_amount",condition_vars)]
t<-apply(t, 2, as.numeric)
cor_matrix <- cor(t)
ggcorrplot(cor_matrix, lab = TRUE, type = "lower", 
           colors = c("red", "white", "blue"),
           title = "Correlation Matrix")

t <- data[c("total_claim_amount", "pet_age_months", "nb_contribution", "nb_excess", "owner_age_years", "nb_number_of_breeds", "nb_average_breed_size", "is_multi_pet_plan")]
t <- apply(t, 2, as.numeric)
cor_matrix <- cor(t)
# condition_eyes and ears 
ggcorrplot(cor_matrix, lab = TRUE, type = "lower", 
           colors = c("red", "white", "blue"),
           title = "Correlation Matrix")
cor(data$total_claim_amount, as.numeric(data$UW_Date)) #nb excess with 0.11

t <- data[data$pet_de_sexed_age != "Not desexed" ,]
cor(t$total_claim_amount, as.numeric(t$pet_de_sexed_age)) #only -0.01867

#densities of claim amounts, for people with multi insurance:
ggplot(data=data) +
  geom_density(mapping = aes(x = total_claim_amount, color=is_multi_pet_plan, linewidth = 2))
# densities for the claim amounts for 3 excess hodlers.
ggplot(data=data %>% mutate(nb_excess = as.factor(nb_excess)) %>% filter(total_claim_amount > 1000)) +
  geom_density(mapping = aes(x = total_claim_amount, color=nb_excess)) +
  labs(title = "Proportional of claims that are large, for different Excesses")
# boxplots on breeds
ggplot(data=data %>% filter(total_claim_amount > 1000)) +
  geom_boxplot(mapping = aes(y = total_claim_amount)) +
  facet_wrap(~ nb_breed_trait) +
  labs(title = "Boxplots for claims > $1000, for different Breeds")


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
