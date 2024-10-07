library(dplyr)

claims_data <- read.csv("UNSW_claims_data.csv")
earned_data <- read.csv("UNSW_earned_data_adjusted_Sep27.csv")

# exposure id is identical to exposure id 1. remove.
earned_data <- earned_data%>%select(-exposure_id_1)

# is there a primary key combination we can use>
length(unique(claims_data$claim_id))
length(unique(claims_data$exposure_id)) # exposureid is not a primary key in claims_data
length(unique(claims_data$claim_id, claims_data$exposure_id))
length(unique(claims_data$claim_id, claims_data$exposure_id))
nrow(claims_data) # claim_id is not a primary key in claims_data

claims_data %>% filter(claim_id == "claim_e379e560-6324-46ae-92a0-a4cfc6bb3525")
claims_data %>% filter(claim_id == "claim_0eb34739-5fd4-4d91-b45e-ea4ca077ffd7")
claims_data %>% filter(claim_id == "claim_2a6f8e5d-a334-4684-ac81-003174cae85d")
claims_data %>% group_by(claim_id) %>% summarise(n=n()) %>% arrange(desc(n))



# duplicated rows in claims_data
as_tibble(claims_data[duplicated(claims_data),])

# money on a claim id
t <- claims_data %>% filter(total_claim_amount >0) %>% select(total_claim_amount, claim_id, exposure_id)
length(unique(t$claim_id, t$exposure_id))
dupeNonZeroClaims <- tibble(t[duplicated(t[,2:3]),]) %>% arrange(claim_id, exposure_id)
# tenures between datasets should be identical.
# tenure + nb_policy_first_inception_date (earneddata) should be the claim_start_date or UW date...
# what is quoting?


tenures <- claims_data %>% select(tenure, exposure_id)
tenures2 <- earned_data %>% select(tenure, exposure_id)
  
t <- merge(x=tenures, y=earned_data, by=c("exposure_id", "tenure"), all.x=TRUE)
