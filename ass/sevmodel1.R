# Create the real density plot using ggplot
ggplot(filter(freMTPL2full[train_indices, ], ClaimNb > 0), aes(x = ClaimTotal)) +
  geom_density(color = "darkblue", linewidth = 0.8) +
  labs(title = "Empirical Density of Claim Amounts",
       x = "Claim Total",
       y = "Density") +
  xlim(0, 10000) + 
  theme_minimal()

# build gamma and inverse gaussian with log link.
# Subset selection
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
