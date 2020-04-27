library(CARBayes)
library(sp)
library(spdep)
library(nnet)
library(MASS)
library(coda)
library(readr)
library(dplyr)

options(scipen = 999)

# For multinomial (no spatial effects): https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# For spatial effects: https://cran.r-project.org/web/packages/CARBayes/vignettes/CARBayes.pdf

# Starts with alldata object from 03_getvars.R.


#
# Prepare ------------------------------------------------------------------------------------------
#

# Read
rundata <- read_rds("./rivanna_data/working/gentri/alldata.Rds")

# Create sp object
rundata <- as(rundata, "Spatial")


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
testmodel <- multinom(type1218 ~ tct_tradfam12 + tct_rentburd12 + tct_medhome12 + tct_medrent12 + 
                        tct_newbuild18 + tct_singfam12 + tct_transit12, data = rundata@data)
summary(testmodel)

testmodel <- multinom(type1218 ~ tct_withba12 + tct_hhinc12 + tct_nonwhite12 + tct_medhome12 + 
                   tct_inpov12 + tct_medrent12 + tct_unemp12 + chg1218_tct_withba + chg1218_tct_hhinc + 
                   chg1218_tct_nonhispwh + tct_multunit12 + tct_diffhou12 + tct_renters12 + tct_nonfam12 + 
                   tct_singfam12 + chg1218_tct_singfam +  tct_vacant12 + chg1218_tct_renters + 
                   chg1218_tct_nonfam + tct_transit12 + tct_popdens12 + tct_newbuild18 + 
                   chg1218_tct_housdens + chg1218_tct_popdens + tct_rentburd12 + chg1218_tct_medhome + 
                   chg1218_tct_popgrowth,
                 data = rundata@data)
stepAIC(testmodel, trace = TRUE)

testmodel <- multinom(formula = type1218 ~ tct_tradfam12 + tct_rentburd12 + tct_medhome12 + tct_medrent12 + 
                        tct_newbuild18 + tct_singfam12 + tct_transit12,
                        data = rundata@data)
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


#
# Non-spatial model (binary logistic) ---------------------------------------------------------
#

# Turns out you can't compute this on a multinomial model because it only works on one set of
# coefficients at a time. Using binary logistic to predict gentrified vs. all others instead.

# Variables
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
# tct_unemp12
# tct_inpov12
# tct_multunit12
# tct_diffhou12
# tct_nonfam12
# tct_singfam12
# tct_vacant12
# tct_tradfam12
# tct_transit12
# tct_rentburd12
# tct_popdens12
# tct_housdens12
# tct_totalpop12

table(rundata$gentrified1218)

# Stepwise
testmodel <- glm(gentrified1218 ~ tct_withba12 + tct_hhinc12 + tct_nonwhite12 + tct_medhome12 + 
                   tct_inpov12 + tct_medrent12 + tct_unemp12 + chg1218_tct_withba + chg1218_tct_hhinc + 
                   chg1218_tct_nonhispwh + tct_multunit12 + tct_diffhou12 + tct_renters12 + tct_nonfam12 + 
                   tct_singfam12 + chg1218_tct_singfam +  tct_vacant12 + chg1218_tct_renters + 
                   chg1218_tct_nonfam + tct_transit12 + tct_popdens12 + tct_newbuild18 + 
                   chg1218_tct_housdens + chg1218_tct_popdens + tct_rentburd12 + chg1218_tct_medhome + 
                   chg1218_tct_popgrowth,
                  data = rundata@data, family = binomial(link = "logit"))

stepAIC(testmodel, trace = TRUE)
summary(testmodel)

# Harris county final model
testmodel <- glm(gentrified1218 ~ tct_tradfam12 + tct_rentburd12 + tct_medhome12 + tct_medrent12 + 
                 tct_newbuild18 + tct_singfam12 + tct_transit12,
                 data = rundata@data, family = binomial(link = "logit"))
summary(testmodel)

# Compute Moran’s I statistic
# To quantify the presence of spatial autocorrelation in the model residuals
# Conduct a permutation test to assess its significance. 
# The permutation test has the null hypothesis of no spatial autocorrelation and an alternative 
# hypothesis of positive spatial autocorrelation.
Wnb <- poly2nb(rundata) # turn data into a neighborhood (nb) object
Wlist <- nb2listw(Wnb, style = "B") # turn data into a listw object, the required form of binary spatial adjacency information (based on border sharing) used by the moran.mc function
moran.mc(x = residuals(testmodel), listw = Wlist, nsim = 5000) # run spatial autocorrelation test

length(resid(testmodel)) 
length(Wlist)

#
# Try a spatial model --------------------------------------------------------
#

# S.CARleroux does not support multinomial models, only S.glm does.

# Create neighborhood matrix
W <- nb2mat(Wnb, style = "B")

# Try MVS.CARleroux
chain1 <- MVS.CARleroux(formula = as.numeric(type1218) ~ tct_tradfam12 + tct_rentburd12 + tct_medhome12 + tct_medrent12 + 
                          tct_newbuild18 + tct_singfam12 + tct_transit12, data = rundata@data, 
                      family = "multinomial", W = W, burnin = 100000, n.sample = 300000, thin = 100, trials = rep(1, nrow(rundata)))
chain2 <- MVS.CARleroux(formula = as.numeric(type1218) ~ tct_tradfam12 + tct_rentburd12 + tct_medhome12 + tct_medrent12 + 
                          tct_newbuild18 + tct_singfam12 + tct_transit12, data = rundata@data, 
                      family = "multinomial", W = W, burnin = 100000, n.sample = 300000, thin = 100, trials = rep(1, nrow(rundata)))
chain3 <- MVS.CARleroux(formula = as.numeric(type1218) ~ tct_tradfam12 + tct_rentburd12 + tct_medhome12 + tct_medrent12 + 
                          tct_newbuild18 + tct_singfam12 + tct_transit12, data = rundata@data, 
                      family = "multinomial", W = W, burnin = 100000, n.sample = 300000, thin = 100, trials = rep(1, nrow(rundata)))


# Inference for this model is based on 3 parallel Markov chains, each of which has been run 
# for 300,000 samples, the first 100,000 of which have been removed as the burn-in period. 
# The remaining 200,000 samples are thinned by 100 to reduce their temporal autocorrelation,
# resulting in 6,000 samples for inference across the 3 Markov chains.

# Try binary logistic model 
chain1 <- S.CARleroux(formula = gentrified1218 ~ tct_rentburd12 + tct_medhome12 + tct_medrent12 + 
                                tct_newbuild18, data = rundata@data, 
                      family = "binomial", W = W, burnin = 100000, n.sample = 300000, thin = 100, trials = rep(1, nrow(rundata)))
chain2 <- S.CARleroux(formula = gentrified1218 ~ tct_rentburd12 + tct_medhome12 + tct_medrent12 + 
                                tct_newbuild18, data = rundata@data, 
                      family = "binomial", W = W, burnin = 100000, n.sample = 300000, thin = 100, trials = rep(1, nrow(rundata)))
chain3 <- S.CARleroux(formula = gentrified1218 ~ tct_rentburd12 + tct_medhome12 + tct_medrent12 + 
                                 tct_newbuild18, data = rundata@data, 
                      family = "binomial", W = W, burnin = 100000, n.sample = 300000, thin = 100, trials = rep(1, nrow(rundata)))

# MCMC sample convergence
summary(chain1$samples)

beta.samples <- mcmc.list(chain1$samples$beta, chain2$samples$beta, chain3$samples$beta)
plot(beta.samples[ , 2:4])

# Potential scale reduction factor (value less than 1.1 is suggestive of convergence)
gelman.diag(beta.samples)