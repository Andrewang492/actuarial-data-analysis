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
claims_data %>% filter(claim_id == "claim_02e2cb40-65e9-4c6a-b25b-799d06c0f7a4")
claims_data %>% filter(claim_id == "claim_2a6f8e5d-a334-4684-ac81-003174cae85d")
claims_data %>% filter(exposure_id == "exposure_4464f014-04bd-47b4-8cc5-1fec8e92b31e") # one exposure can have different claims.
claims_data %>% group_by(claim_id) %>% summarise(n=n()) %>% arrange(desc(n))



# duplicated rows in claims_data
as_tibble(claims_data[duplicated(claims_data),])

# money on a claim id
t <- claims_data %>% filter(total_claim_amount >0) %>% select(total_claim_amount, claim_id, exposure_id)
length(unique(t$claim_id, t$exposure_id))
dupeNonZeroClaims <- tibble(t[duplicated(t[,2:3]),]) %>% arrange(claim_id, exposure_id) # 'split' claims can have their amounts distributed across entries.
# tenures between datasets should be identical...
# tenure + nb_policy_first_inception_date (earneddata) should be the claim_start_date or UW date...
# what is quoting?


tenures <- claims_data %>% select(tenure, exposure_id)
tenures2 <- earned_data %>% select(tenure, exposure_id)
  
t <- merge(x=tenures, y=earned_data, by=c("exposure_id", "tenure"), all.x=TRUE)

# why are there multiple exposure entries in earned_data? One per tenure month.
t1 <- earned_data %>% filter(duplicated(earned_data$exposure_id)) %>% arrange(exposure_id) #note that the first entry will not be shown (usually tenure 0)
View(earned_data %>% filter(exposure_id == "exposure_000e4bc1-8623-43a5-a47f-3685d8d084be"))
# reduce the rows - select max tenure and first uw_date
exposures <- earned_data %>% group_by(exposure_id) %>% summarise(max_tenure = max(tenure), first_uw_date = min(UW_Date), total_earned_units = sum(earned_units), across(-c(UW_Date, tenure, earned_units), first))
