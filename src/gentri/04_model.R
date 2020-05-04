library(nnet)
library(MASS)
library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)

options(scipen = 999)

# For multinomial (no spatial effects): https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/

# Starts with alldata object from 03_getvars.R.


#
# Prepare ------------------------------------------------------------------------------------------
#

# Read
rundata <- read_rds("./rivanna_data/working/gentri/alldata.Rds")


#
# Non-spatial model (multinomial logistic) ---------------------------------------------------------
#

# Select baseline outcome level
rundata$type1218 <- factor(rundata$type1218, 
                           levels = c("Not vulnerable", "Vulnerable, did not gentrify", "Vulnerable, gentrified"))
rundata$type1218 <- relevel(rundata$type1218, ref = "Vulnerable, did not gentrify")

# Model
# Output includes some iteration history and includes the final log-likelihood.
# This value multiplied by two is then seen in the model summary as the Residual Deviance 
# and it can be used in comparisons of nested models.

testmodel <- multinom(type1218 ~ tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + chg1218_tct_renters + 
                        chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                        chg1218_tct_popgrowth + chg1218_tct_housdens,
                 data = rundata)
# stepAIC(testmodel, trace = TRUE)

summary(testmodel)

# Display as risk ratios (extract coefficients from the model and exponentiate)
exp(coef(testmodel))


# Calculate Z scores
# The multinom command does not include p-value calculation for the regression coefficients, 
# so we calculate p-values using Wald tests (here z-tests).
z <- summary(testmodel)$coefficients/summary(testmodel)$standard.errors
z

# 2-tailed Z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Calculate predicted probabilities
head(pp <- fitted(testmodel))

# Unlike logistic regression where there are many statistics for performing model diagnostics, 
# it is not as straightforward to do diagnostics with multinomial logistic regression models. 
# For the purpose of detecting outliers or influential data points, one can run separate logit 
# models and use the diagnostics tools on each model.


#
# Plot -----------------------------------------------------------------------------
#

# Look at the averaged predicted probabilities for different values of the predictor variables.

dfpred <- data.frame(chg1218_tct_medrent_pct = rep(c(-20:50)), chg1218_tct_renters = mean(rundata$chg1218_tct_renters),
                     tct_diffhou12 = mean(rundata$tct_diffhou12), tct_newbuild18 = mean(rundata$tct_newbuild18), 
                     tct_multunit12 = mean(rundata$tct_multunit12), tct_transit12 = mean(rundata$tct_transit12), 
                     chg1218_tct_medhome_pct = mean(rundata$chg1218_tct_medhome_pct), 
                     chg1218_tct_singfam = mean(rundata$chg1218_tct_singfam), 
                     chg1218_tct_popgrowth = mean(rundata$chg1218_tct_popgrowth),
                     chg1218_tct_housdens = mean(rundata$chg1218_tct_housdens))

# store the predicted probabilities for each value of chg1218_tct_medrent_pct and chg1218_tct_renters
pprobs <- cbind(dfpred, predict(testmodel, newdata = dfpred, type = "probs", se = TRUE))

# calculate the mean probabilities within each level of chg1218_tct_medrent_pct
by(pprobs[, 11:13], pprobs$chg1218_tct_medrent_pct, colMeans)

# reshape
lpp <- melt(pprobs, id.vars = c("chg1218_tct_medrent_pct"), value.name = "probability")
head(lpp)  # view first few rows
lpp <- lpp[640:852, ]

# plot
ggplot(lpp, aes(x = chg1218_tct_medrent_pct, y = probability)) + 
  geom_line() + 
  facet_grid(variable ~ ., scales = "free")

# Look at:
# https://thomasleeper.com/Rcourse/Tutorials/nominalglm.html
# https://cran.r-project.org/web/packages/MNLpred/vignettes/OVA_Predictions_For_MNL.html


#
# Variables ---------------------------------------------------------
#

# tct_withba12
# tct_nonwhite12
# tct_hhinc12
# tct_medhome12
# tct_medrent12
# chg1218_tct_withba
# chg1218_tct_hhinc
# chg1218_tct_nonhispwh
# tct_renters12
# chg1218_tct_medhome
# chg1218_tct_singfam
# chg1218_tct_nonfam
# chg1218_tct_popdens
# chg1218_tct_housdens
# chg1218_tct_popgrowth
# chg1218_tct_renters
# tct_singfam18
# tct_nonfam18
# tct_popdens18
# tct_housdens18
# tct_newbuild18
# tct_totalpop18
# tct_unemp12 * 
# tct_inpov12 *
# tct_multunit12
# tct_diffhou12
# tct_nonfam12
# tct_singfam12
# tct_vacant12 * 
# tct_tradfam12
# tct_transit12
# tct_rentburd12 *
# tct_popdens12
# tct_housdens12
# tct_totalpop12


#
# Try a spatial model --------------------------------------------------------
#

# Compute Moran’s I statistic
# To quantify the presence of spatial autocorrelation in the model residuals
# Conduct a permutation test to assess its significance. 
# The permutation test has the null hypothesis of no spatial autocorrelation and an alternative 
# hypothesis of positive spatial autocorrelation.
Wnb <- poly2nb(rundata) # turn data into a neighborhood (nb) object
Wlist <- nb2listw(Wnb, style = "B") # turn data into a listw object, the required form of binary spatial adjacency information (based on border sharing) used by the moran.mc function
moran.mc(x = residuals(testmodel), listw = Wlist, nsim = 5000) # run spatial autocorrelation test
