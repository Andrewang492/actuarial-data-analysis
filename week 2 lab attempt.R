library(CASdatasets)
library(ggplot2)
library(tidyverse)

data(freMTPL2freq)
data(freMTPL2sev)
head(freMTPL2freq)

freMTPL2freq$ClaimNb <- as.integer(freMTPL2freq$ClaimNb)
summary(freMTPL2freq$ClaimNb)
freMTPL2freq$Exposure

#---- case study a ----

# no NA
sum(is.na(freMTPL2freq))

# distribution of claimnb and exposure

ggplot(data= (freMTPL2freq %>% filter(ClaimNb > 2))) +
  geom_histogram(mapping=aes(x=ClaimNb))
summary(freMTPL2freq$ClaimNb)

ggplot(data=freMTPL2freq) +
  geom_histogram(mapping=aes(x=Exposure), bins=1000) +
  geom_freqpoly(mapping=aes(x=Exposure, y=..count..), bins=100, color='red') +
  coord_cartesian(ylim=c(0,10000))
summary(freMTPL2freq$Exposure)
count(freMTPL2freq, Exposure);

#  is Area categorical
count(freMTPL2freq, Area)
levels(freMTPL2freq$Area)

# age and claim frequency

# raw claim numbers:
ggplot(data=freMTPL2freq) +
  geom_smooth(mapping=aes(x=DrivAge, y=ClaimNb))
# claim number/ exposure:
freMTPL2freq %>% mutate(frequency = ClaimNb/Exposure) %>%
  ggplot() +
  geom_smooth(mapping=aes(x=DrivAge, y=frequency))
freMTPL2freq <- freMTPL2freq %>% mutate(frequency = ClaimNb/Exposure)

# correlations:
cor(freMTPL2freq$ClaimNb, freMTPL2freq$Exposure)
cor(freMTPL2freq[c('ClaimNb', 'Exposure', 'VehPower', 'VehAge', 'DrivAge', 'BonusMalus', 'Density')])
# drivage has a high negative correlation with bonus malus.
# drivage also has high correlation (0.17) with Exposure.
# Bonus malus with Exposure
# They can indicate relationships towards exposure, or they can show that one is redundant.
# cannot use geom_point because there are too many observations
#ggplot(data=freMTPL2freq) +
 # geom_point(mapping = aes(x=Region, y=frequency))
 # 

#---- case study b----

credit <- readxl::read_xls('credit.xls')
colnames(credit) <- credit[1,]
credit <- credit[2:nrow(credit), ]
sum(is.na(credit)) #no NA
credit1 <- lapply(credit, as.numeric)
credit2 <- as_tibble(credit1)
ggplot(data=credit2) +
  geom_boxplot(mapping=aes(x=AGE))
# one issue is no Pay 1 but there is Pay 0.

# if -1 means paid duly, then what is -2???
# how are people paying bills duly without paying anything? record 2.
# assume that is the missing data.
credit2 %>%filter(PAY_0 == -1, PAY_AMT1 == 0)

# only fix payment 1's. Make it average of other payments:
credit3 <- credit2 %>% filter(PAY_0 == -1, PAY_AMT1 == 0) %>%
  mutate(PAY_AMT1 = (PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6)/5)


as_tibble(cov(credit2))
ggplot(data= credit2) + 
  geom_point(mapping=aes(x=AGE, y=PAY_0))

# you might want to have a column
pivot_longer(credit2, cols=c("PAY_0","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6"), names_to = "PAY_NO", values_to="PAY_STATUS") %>%
  pivot_longer(cols=starts_with("PAY_AMT"), names_to = "PAY_AMT_NO", values_to="PAY_AMT")

# default being correlated with others....
colnames(credit2)[25] <- 'default'
