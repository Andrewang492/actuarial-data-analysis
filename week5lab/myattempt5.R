#Load the required packages
library(CASdatasets)
library(tidyverse)
library(MASS) #stepAIC and glm.nb()
library(statmod)
library(MLmetrics)

data("freMTPL2freq")
data("freMTPL2sev")
#Data pre-processing
freq_data <- freMTPL2freq[, -2] # Remove the column of ClaimNb from the frequency data
freq_data$VehGas <- factor(freq_data$VehGas)
sev_data <- freMTPL2sev
sev_data$ClaimNb <- 1
agg_sev <- aggregate(sev_data, by=list(IDpol=sev_data$IDpol), FUN = sum)[c(1,3:4)]
names(agg_sev)[2] <- "ClaimTotal"
merged_data <- merge(x=freq_data, y=agg_sev, by="IDpol", all.x=TRUE)
merged_data[is.na(merged_data)] <- 0
merged_data <- merged_data[merged_data$ClaimNb <= 5, ]
merged_data$Exposure <- pmin(merged_data$Exposure, 1) #ones too large are ceilinged to 1.
freMTPL2full <- merged_data


#----claim count lesss than 24500----
#...do not have severity entries. Therefore use claim counts in the second file.
a <- freMTPL2freq %>% filter(IDpol < 24500)
b <- merge(freMTPL2freq, a, by = "IDpol")

# ids that don't exist in sev. Many. So it is true, maybe it's a good idea to use claim counts from freMTPL2sev
a$IDpol[!(a$IDpol %in% freMTPL2sev$IDpol)]

# the cleaning above aggregates the severity, making a count and a total per policy.
#----

#Split the Data
set.seed(903)
train_indices <- sample(1:nrow(freMTPL2full), size = 0.7 * nrow(freMTPL2full))
train_data <- freMTPL2full[train_indices, ]
test_data <- freMTPL2full[-train_indices, ]

glm(ClaimNb ~ . -IDpol, data = train_data, offset = log(Exposure), family = poisson(link = "log"))


#---- severity ----
# gamma, inverse gaussian, with log link for interpretability.
# fit Gamma full. Total claim per policy, offset by claim count. Set claim count as the glm weight.
# why both weight and offset????

mygammasev <- glm(ClaimTotal ~ Exposure + VehPower + VehAge + DrivAge + BonusMalus + VehBrand +
      VehGas + Area + Density + Region, data = train_data[train_data$ClaimNb > 0, ],
    offset = log(ClaimNb), weights= ClaimNb, family = Gamma(link = "log")) # is dividing claimNB the same as weights??
summary(mygammasev)

# ---- validation and evaluation----

y <- tibble(test_data[test_data$ClaimNb >0, ]) # remove 0 claimNBs right?!!!!
predictions <- predict.glm(mygammasev, newdata = y, type="response")
# RMSE
RMSE(predictions, y$ClaimTotal) #8664.295 Do I need to divide claimNB?? Do i need to use two different datasets?
RMSE(predictions / y$ClaimNb, y$ClaimTotal/y$ClaimNb) #8558.623
#Deviance
deviance(mygammasev)
# Gamma deviance for each observation
gamma_deviance <- ((y$ClaimTotal - predictions) / predictions - log(y$ClaimTotal / predictions))
total_deviance <- 2 * sum(gamma_deviance)
total_deviance # 10981.19
# AIC
AIC(mygammasev)
BIC(mygammasev)
# Spearman correlation
cor(predictions / y$ClaimNb, y$ClaimTotal / y$ClaimNb, method = "spearman") #0.08377
cor(predictions, y$ClaimTotal, method = "spearman") # 0.1598
#chisq. Not appropriate?
chisq.test(x = predictions, y = y$ClaimTotal) #X-squared = 28275474, df = 28271672, p-value = 0.3065
chisq.test(x = predictions / y$ClaimNb, y = y$ClaimTotal/y$ClaimNb) #X-squared = 28268037, df = 28267870, p-value = 0.4911
# F Test
anova(mygammasev, test="F")
# Deciles test (very difficult)
# Randomised quantile Residuals (difficult)
# qq plot (on residuals)
plot(sevGamma_full$fitted.values,rstandard(sevGamma_full),
     xlab="Fitted values",ylab="Standardized deviance residuals",
     main="SDR vs FV, Gamma")
abline(h=0,col="red",lty=2)

qqnorm(rstandard(sevGamma_full))
qqline(rstandard(sevGamma_full),col="red")
