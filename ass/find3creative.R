library(tidyverse)
merged <- read_csv("Data/merged.csv")
merged$pet_de_sexed_age <- as.factor(merged$pet_de_sexed_age)
summary(merged$pet_de_sexed_age)

#### exploration ####
merged %>% select(claim_start_date, nb_policy_first_inception_date, UW_Date, quote_date)
merged %>% select(claim_id, claim_start_date, nb_policy_first_inception_date, UW_Date, quote_date) %>%
  filter(claim_start_date < nb_policy_first_inception_date) # one claim made claim before getting, invalid times, to remove.
# claim_c5f5cbc0-563f-42cd-851f-d9783ebd6442
