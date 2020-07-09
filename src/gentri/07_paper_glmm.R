library(ngspatial)
library(sp)
library(spdep)
library(nnet)
library(MASS)
library(coda)
library(readr)
library(dplyr)
library(gclus)
library(ggplot2)

options(scipen = 999)

# Starts with alldata object from 03_getvars.R.


#
# Prepare ------------------------------------------------------------------------------------------
#

# Read
rundata <- read_rds("./rivanna_data/working/gentri/alldata.Rds")
rundata <- rundata %>% arrange(GEOID)

# Create sp object
rundata <- as(rundata, "Spatial")

# Create other objects
Wnb <- poly2nb(rundata) # turn data into a neighborhood (nb) object
W <- nb2mat(Wnb, style = "B") # create neighborhood matrix


#
# Test for spatial autocorrelation ----------------------------------------------------------------------------------
#

# https://rpubs.com/chrisbrunsdon/part2

# turn data into a listw object, the required form of binary spatial adjacency information 
# (based on border sharing) used by the moran.mc and jointcount.mc functions
Wlist <- nb2listw(Wnb, style = "B") 

# A permutation test for same colour join count statistics calculated by using nsim random 
# permutations of fx for the given spatial weighting scheme, to establish the ranks of the 
# observed statistics (for each colour) in relation to the nsim simulated values.
# Null hypothesis: no clustering.
joincount.mc(as.factor(rundata@data$type1218), Wlist, nsim = 1000)

# test for spatial autocorrelation using a spatial weights matrix in weights list form for
# testing whether same-colour joins occur more frequently than would be expected if the zones
# were labelled in a spatially random way. The assumptions underlying the test are sensitive
# to the form of the graph of neighbour relationships and other factors, and results may be
# checked against those of joincount.mc permutations.
# Null hypothesis: no clustering.
joincount.test(as.factor(rundata@data$type1218), Wlist)


#
# Inspect ---------------------------------------------------------------------------------
#

cormatdata <- rundata@data %>% select(chg1218_tct_withba,
                                      chg1218_tct_hhinc_pct,
                                      chg1218_tct_nonhispwh,
                                      chg1218_tct_medrent_pct,
                                      chg1218_tct_medhome_pct,
                                      chg1218_tct_singfam,
                                      chg1218_tct_nonfam,
                                      chg1218_tct_popdens,
                                      chg1218_tct_housdens,
                                      chg1218_tct_popgrowth,
                                      chg1218_tct_renters)

cormat <- cov(cormatdata) 
cpairs(cormatdata, order.single(cormat), dmat.color(cormat))


# 
# Try sparse.sglmm -----------------------------------------------------------------------------------------------
#

# This function fits the sparse restricted spatial regression model of Hughes and Haran (2013), 
# or the Bayesian spatial filtering model of Hughes (2017). 
# If the response is Bernoulli, negative binomial, or Poisson, β and γ are updated using 
# Metropolis-Hastings random walks with normal proposals. 
# The proposal covariance matrix for β is the estimated asymptotic covariance matrix from a glm fit to the data.

# Create outcome variable
rundata@data <- rundata@data %>% mutate(c1_notvul = ifelse(type1218 == "Not vulnerable", 1, 0),
                                        c2_vulnotg = ifelse(type1218 == "Vulnerable, did not gentrify", 1, 0),
                                        c3_vulg = ifelse(type1218 == "Vulnerable, gentrified", 1, 0))

rundata@data$c4_vul <- rundata@data$c2_vulnotg + rundata@data$c3_vulg
# binomial outcomes: c3_vulg, c4_vul

# Set seed
set.seed(76978)

# need this for sparse.sglmm to recognize W is symmetric
rownames(W) <- colnames(W) 

# We use, and recommend, fixed-width output analysis (Flegal et al., 2008). In fixed-width analysis,
# one chooses a tolerance and stops sampling when all Monte Carlo standard errors fall below the
# tolerance. The output shown below indicates that the Monte Carlo standard errors fell below the
# default tolerance of 0.01 after 714,025 draws were made from the posterior, which is why the analysis
# was terminated before having reached the (default) maximum number of iterations (maxit = 1e6).
# We use the batchmeans package (Haran and Hughes, 2012) to compute Monte Carlo standard errors
# (denoted MCSE in the output).

# Tolerance: Reasonable levels are anywhere from .01 to .05. 
# The smaller the tolerance, the larger the minimum effective samples.
# The relative tolerance level can be chosen according to the level of
# precision in the estimates desired, typically ≤ .05. Once effective sample size reaches the cutoff, users can
# stop simulating since they can be 100(1 − α)% confident that estimates are within an Xth
# level of tolerance relative to the target distribution.

# Effective sample size: The ESS encapsulates the idea that not all of our
# samples contribute the same amount of information: if we have n samples that are very
# similar to each other, we expect to have a substantially worse estimate than if we have n
# samples that are quite different. This is because the information in correlated samples are
# at least partially redundant with one another, with the amount of redundancy increasing
# with the strength of the correlation: while two independent samples provide completely
# unique information about the distribution and no information about each other, two correlated samples instead provide some information about each other at the expense of the
# underlying distribution.

# Outcome = Vulnerable and gentrified
chain1.binG <- sparse.sglmm(formula = c3_vulg ~ 
                              chg1218_tct_multunit + chg1218_tct_vacant +
                              chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                              chg1218_tct_renters + chg1218_tct_medrent_pct + 
                              chg1218_tct_housdens + 
                              chg1218_tct_rentburd + chg1218_tct_diffhou + 
                              chg1218_tct_transit + 
                              chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                              chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                              chg1218_tct_popgrowth,
                            data = rundata@data,
                            method = "RSR",
                            family = "binomial", A = W,
                            minit = 5000, maxit = 1e6, tol = 0.01, verbose = T) # full run: maxit=1e6, otherwise e5

chain2.binG <- sparse.sglmm(formula = c3_vulg ~ 
                              chg1218_tct_multunit + chg1218_tct_vacant +
                              chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                              chg1218_tct_renters + chg1218_tct_medrent_pct + 
                              chg1218_tct_housdens + 
                              chg1218_tct_rentburd + chg1218_tct_diffhou + 
                              chg1218_tct_transit + 
                              chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                              chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                              chg1218_tct_popgrowth,
                            data = rundata@data,
                            method = "RSR",
                            family = "binomial", A = W,
                            minit = 5000, maxit = 1e6, tol = 0.01, verbose = T) # full run: maxit=1e6, otherwise e5

chain3.binG <- sparse.sglmm(formula = c3_vulg ~ 
                              chg1218_tct_multunit + chg1218_tct_vacant +
                              chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                              chg1218_tct_renters + chg1218_tct_medrent_pct + 
                              chg1218_tct_housdens + 
                              chg1218_tct_rentburd + chg1218_tct_diffhou + 
                              chg1218_tct_transit + 
                              chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                              chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                              chg1218_tct_popgrowth,
                            data = rundata@data,
                            method = "RSR",
                            family = "binomial", A = W,
                            minit = 5000, maxit = 1e6, tol = 0.01, verbose = T) # full run: maxit=1e6, otherwise e5

# Outcome: vulnerable not gentrified
chain1.binVng <- sparse.sglmm(formula = c2_vulnotg ~ 
                              chg1218_tct_multunit + chg1218_tct_vacant +
                              chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                              chg1218_tct_renters + chg1218_tct_medrent_pct + 
                              chg1218_tct_housdens + 
                              chg1218_tct_rentburd + chg1218_tct_diffhou + 
                              chg1218_tct_transit + 
                              chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                              chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                              chg1218_tct_popgrowth,
                            data = rundata@data,
                            method = "RSR",
                            family = "binomial", A = W,
                            minit = 5000, maxit = 1e6, tol = 0.01, verbose = T) # full run: maxit=1e6, otherwise e5

chain2.binVng <- sparse.sglmm(formula = c2_vulnotg ~ 
                                chg1218_tct_multunit + chg1218_tct_vacant +
                                chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                                chg1218_tct_renters + chg1218_tct_medrent_pct + 
                                chg1218_tct_housdens + 
                                chg1218_tct_rentburd + chg1218_tct_diffhou + 
                                chg1218_tct_transit + 
                                chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                                chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                                chg1218_tct_popgrowth,
                              data = rundata@data,
                              method = "RSR",
                              family = "binomial", A = W,
                              minit = 5000, maxit = 1e6, tol = 0.01, verbose = T) # full run: maxit=1e6, otherwise e5

chain3.binVng <- sparse.sglmm(formula = c2_vulnotg ~ 
                                chg1218_tct_multunit + chg1218_tct_vacant + 
                                chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                                chg1218_tct_renters + chg1218_tct_medrent_pct + 
                                chg1218_tct_housdens + 
                                chg1218_tct_rentburd + chg1218_tct_diffhou + 
                                chg1218_tct_transit + 
                                chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                                chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                                chg1218_tct_popgrowth,
                              data = rundata@data,
                              method = "RSR",
                              family = "binomial", A = W,
                              minit = 5000, maxit = 1e6, tol = 0.01, verbose = T) # full run: maxit=1e6, otherwise e5

# Save models
saveRDS(chain1.binG, file = "./rivanna_data/working/paper/chain1.binG.glmm.Rds")
saveRDS(chain2.binG, file = "./rivanna_data/working/paper/chain2.binG.glmm.Rds")
saveRDS(chain3.binG, file = "./rivanna_data/working/paper/chain3.binG.glmm.Rds")

saveRDS(chain1.binVng, file = "./rivanna_data/working/paper/chain1.binVng.glmm.Rds")
saveRDS(chain2.binVng, file = "./rivanna_data/working/paper/chain2.binVng.glmm.Rds")
saveRDS(chain3.binVng, file = "./rivanna_data/working/paper/chain3.binVng.glmm.Rds")

#
# Read in chains ----------------------------------------------------------------------
#

chain1.binG <- readRDS("./rivanna_data/working/paper/chain1.binG.glmm.Rds")
chain2.binG <- readRDS("./rivanna_data/working/paper/chain2.binG.glmm.Rds")
chain3.binG <- readRDS("./rivanna_data/working/paper/chain3.binG.glmm.Rds")

chain1.binVng <- readRDS(file = "./rivanna_data/working/paper/chain1.binVng.glmm.Rds")
chain2.binVng <- readRDS(file = "./rivanna_data/working/paper/chain2.binVng.glmm.Rds")
chain3.binVng <- readRDS(file = "./rivanna_data/working/paper/chain3.binVng.glmm.Rds")


#
# Output ----------------------------------------------------------------------
#

# This function displays (1) the call to sparse.sglmm, (2) the values of the hyperparameters and
# tuning parameters, (3) a table of estimates, (4) the DIC value for the fit, and (5) the number of
# posterior samples. Each row of the table of estimates shows an estimated regression coefficient, the
# HPD interval for the coefficient, and the Monte Carlo standard error.
# CIs: "There is a probability of 0.95 that the interval contains μ"

# Typically report either the mean or median of the posterior samples for each parameter of interest as a point estimate
# 2.5% and 97.5% percentiles of the posterior sample for each parameter give a 95% posterior credible interval (interval
# within which the parameter lies with probability 0.95)

# Tuning parameter: sigma.s = standard deviations for the γ proposal (γ are spatial random effects). 
# The proposal for γ is spherical normal with common standard deviation sigma.s.

# Hyperparameter: sigma.b = prior standard deviation for β (β is a p-vector of regression coefficients)
# The prior for β is spherical p-variate normal with mean zero and common standard deviation sigma.b, which defaults to 1,000. 

summary(chain1.binG)
summary(chain2.binG)
summary(chain3.binG)

summary(chain1.binVng)
summary(chain2.binVng)
summary(chain3.binVng)

# Combine results: Vulnerable and gentrified
beta.samples.matrixG <- rbind(chain1.binG$beta.sample, chain2.binG$beta.sample, chain3.binG$beta.sample)
# Then posterior medians and 95% credible intervals can be computed as follows:
round(t(apply(beta.samples.matrixG, 2, quantile, c(0.5, 0.025, 0.975))), 5)

# Combine results: Vulnerable but not gentrified
beta.samples.matrixVng <- rbind(chain1.binVng$beta.sample, chain2.binVng$beta.sample, chain3.binVng$beta.sample)
# Then posterior medians and 95% credible intervals can be computed as follows:
round(t(apply(beta.samples.matrixVng, 2, quantile, c(0.5, 0.025, 0.975))), 5)


# 
# Model diagnostics -----------------------------------------------------------------------------------------------
#

# Convergence is the idea that, while our estimates using n samples (grid points) might be noisy, it approaches
# some fiducial value as n → ∞.
# Consistency is subsequently the idea that the value we converge to is the true value we
# are interested in estimating.

# MCMC: Slow but produces unbiased estimates, normality not assumed, easy to calculate confidence intervals, 
# but you have to judge convergence by yourself.
# MCMC algorithm converges stochastically to the equilibrium probability distribution.

# If we can write down an analytical expression for the posterior distribution then we can use Gibbs sampling
# If we can’t write down an analytical expression for the posterior then we use Metropolis‐Hastings sampling

# https://www.ukdataservice.ac.uk/media/307220/presentation4.pdf

# Extract residuals
residuals(chain1.binG)
residuals(chain2.binG)
residuals(chain3.binG)

plot(chain1.binG$residuals, chain1.binG$fitted.values)
plot(chain2.binG$residuals, chain2.binG$fitted.values)
plot(chain3.binG$residuals, chain3.binG$fitted.values)

residuals(chain1.binVng)
residuals(chain2.binVng)
residuals(chain3.binVng)

plot(chain1.binVng$residuals, chain1.binVng$fitted.values)
plot(chain2.binVng$residuals, chain2.binVng$fitted.values)
plot(chain3.binVng$residuals, chain3.binVng$fitted.values)

# Deviance Information Criterion
# Diagnostic for model comparison. Goodness of fit criterion that is penalized for model complexity.
# Criterion that can be used with MCMC and which for a linear regression model is equivalent to the AIC 
# A natural way to compare models is to use a criterion based on a trade‐off between the fit of the data 
# to the model and the corresponding complexity of the model. DIC does this in a Bayesian way.
# Models with smaller DIC are better supported by the data.
# Valuable in MLwiN for testing improved goodness of fit of non‐linear model (eg Logit)
# because Likelihood (and hence Deviance is incorrect)
# any decrease in DIC suggests a better model
# Model with delta value within 1-2 of the best model has substantial support in the data and should be 
# considered along with the best model. A delta value within 4-7 has considerably less support. 
# A value >10 indicates that the worse model has virtually no support and can be ommitted from further consideration.

# Deviance Information Criterion
# Fit + Complexity; Dbar + pD
chain1.binG$dic

# Goodness of fit component of the Deviance Information Criterion
# = the average deviance from the complete set of iterations
chain1.binG$D.bar 

# Penalty component of the Deviance Information Criterion
# = the Estimated degrees of freedom consumed in the fit
chain1.binG$pD

# Covariance matrix of the regression parameters
vcov(chain1.binG)

# Accuracy of the posterior estimates can be assessed by
# the Monte Carlo standard error (MCSE) for each parameter.
# The Monte Carlo Standard Error (MCSE) is an indication of how much error is
# in the estimate due to the fact that MCMC is used. As the number of iterations increases the MCSE approaches 0.
plot(chain1.binG$beta.mcse)
plot(chain2.binG$beta.mcse)
plot(chain3.binG$beta.mcse)

plot(chain1.binVng$beta.mcse)
plot(chain2.binVng$beta.mcse)
plot(chain3.binVng$beta.mcse)

plot(chain1.binG$fitted.values)
plot(chain1.binG$linear.predictors)
plot(chain1.binG$residuals)
plot(chain1.binG$gamma.mcse)

# Assess MCMC sample convergence
vgent1 <- mcmc(chain1.binG$beta.sample)
vgent2 <- mcmc(chain2.binG$beta.sample)
vgent3 <- mcmc(chain3.binG$beta.sample)
  
vngent1 <- mcmc(chain1.binG$beta.sample)
vngent2 <- mcmc(chain2.binG$beta.sample)
vngent3 <- mcmc(chain3.binG$beta.sample)

# 2000 x 19 (intercept + 18 variables)
beta.samplesG <- mcmc.list(vgent1, vgent2, vgent3)
beta.samplesVng <- mcmc.list(vngent1, vngent2, vngent3)

# For an individual chain, by variable
plot(chain1.binG$beta.sample[, 13], type = "l")

# For posterior distribution of each of the three chains, by variable
plot(beta.samplesG[, 15])
plot(beta.samplesVng[, 18])

beta.samplesGall <- mcmc(rbind(chain1.binG$beta.sample, chain2.binG$beta.sample, chain3.binG$beta.sample))
beta.samplesVall <- mcmc(rbind(chain1.binVng$beta.sample, chain2.binVng$beta.sample, chain3.binVng$beta.sample))
HPDinterval(beta.samplesGall)
HPDinterval(beta.samplesVall)

# Potential scale reduction factor
# Total value less than 1.1 is suggestive of convergence.
# For covariates pproximate convergence is diagnosed when the upper limit is close to 1.
# A factor of 1 means that between variance and within chain variance are equal, 
# larger values mean that there is still a notable difference between chains.
# The gelman plot shows  the development of the scale-reduction over time (chain steps).
gelman.diag(beta.samplesG)
gelman.plot(beta.samplesG) #these should not be going back up

gelman.diag(beta.samplesVng)
gelman.plot(beta.samplesVng)



#
# Output ------------------------------------------------------------------------------------------
#

# trace plots of (beta.samples) for each of 10 variables (5x2) for the two models
# posterior means + HPD credible intervals for each of the 10 variables for the two models (tables)

# show ability to do model predictions under new covariates


#
# Compare to nonspatial models ------------------------------------------------------------------------------------------
#

chain3.binG.nonspatial <- glm(formula = c3_vulg ~ 
                                tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
                                chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                                chg1218_tct_popgrowth + chg1218_tct_housdens,
                              data = rundata@data,
                              family = "binomial")
confint(chain3.binG.nonspatial)
data.frame( coef(chain3.binG.nonspatial) )

chain3.binV.nonspatial <- glm(formula = c4_vul ~ 
                                tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
                                chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                                chg1218_tct_popgrowth + chg1218_tct_housdens,
                              data = rundata@data,
                              family = "binomial")
confint(chain3.binV.nonspatial)
data.frame( coef(chain3.binV.nonspatial) )


#
# Trace plots ------------------------------------------------------------------------------------------
#

pdf("~/git/fairfax/src/gentri/trace_binG.pdf",height=6,width=4)
par(mar=c(0,0,0,0))
plot(beta.samplesG[,1:11])
dev.off()

pdf("~/git/fairfax/src/gentri/trace_binV.pdf",height=6,width=4)
par(mar=c(0,0,0,0))
plot(beta.samplesV[,1:11])
dev.off()



#
# AUC, GOF ------------------------------------------------------------------------------------------
#

library(pROC)

pdf("~/git/fairfax/src/gentri/auc.pdf",width=8,height=4)
par(mfrow=c(1,2))
plot( roc(response = rundata@data$c3_vulg, predictor = chain1.binG$fitted.values), main = "Outcome: Vulnerable and gentrified" )
text(x = 0.4, y = 0.4,labels="AUC = 0.88")

plot( roc(response = rundata@data$c2_vulnotg, predictor = chain1.binVng$fitted.values), main = "Outcome: Vulnerable but not gentrified" )
text(x = 0.4, y = 0.4, labels = "AUC = 0.82")
dev.off()



#
# Model predictions ------------------------------------------------------------------------------------------
#

chain1.binG$fitted.values # model estimated probability of each tract to gentrify

# predict( chain1.binG ) # no 'predict' method for CARBayes; need to do it manually...

# Run non-spatial
predmodel <- glm(formula = c3_vulg ~ 
                   tct_multunit12 + tct_vacant12 +  tct_newbuild18 + 
                   chg1218_tct_singfam + chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + 
                   chg1218_tct_housdens + chg1218_tct_popgrowth + 
                   tct_rentburd12 + tct_diffhou12 + tct_transit12 +  tct_unemp12 + tct_inpov12 + 
                   chg1218_tct_withba + chg1218_tct_nonhispwh + chg1218_tct_nonfam + chg1218_tct_hhinc_pct,
                 data = rundata@data,
                 family = "binomial")
confint(predmodel)
data.frame( coef(predmodel) )

# Create new DF holding constant and manipulating key var


predmodelV <- glm(formula = c4_vul ~ 
                    tct_multunit12 + tct_vacant12 +  tct_newbuild18 + 
                    chg1218_tct_singfam + chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + 
                    chg1218_tct_housdens + chg1218_tct_popgrowth + 
                    tct_rentburd12 + tct_diffhou12 + tct_transit12 +  tct_unemp12 + tct_inpov12 + 
                    chg1218_tct_withba + chg1218_tct_nonhispwh + chg1218_tct_nonfam + chg1218_tct_hhinc_pct,
                  data = rundata@data,
                  family = "binomial")
confint(predmodelV)
data.frame( coef(predmodelV) )


#
# REVISED Model predictions: Housing price ------------------------------------------------------
#

# reduce the change in home prices by X percent (0 to 50 in intervals of 5)
reduce <- seq(0,25,by=2.5)

pred_probs <- matrix(NA,nrow=nrow(rundata@data),ncol=length(reduce))
for(i in 1:length(reduce)){
  newdata_houseprice <- rundata@data
  newdata_houseprice$chg1218_tct_medhome_pct <- newdata_houseprice$chg1218_tct_medhome_pct - reduce[i]
  pred_probs[,i] <- predict(predmodel, newdata = newdata_houseprice, type = "response")
}

# do the same but for vulnerable tracts
pred_probsV <- matrix(NA,nrow=nrow(rundata@data),ncol=length(reduce))
for(i in 1:length(reduce)){
  newdata_houseprice <- rundata@data
  newdata_houseprice$chg1218_tct_medhome_pct <- newdata_houseprice$chg1218_tct_medhome_pct - reduce[i]
  pred_probsV[,i] <- predict(predmodelV, newdata = newdata_houseprice, type = "response")
}

# look at the intervention effects on fitted probabilities: 10% reduction in housing costs (reduce[5]=10)
# see which tracts that would gentrify will no longer gentrify
sum( pred_probs[,1] > .5 ) # 35 tracts
sum( pred_probs[,5] > .5 ) # 19 tracts
which(pred_probs[,1] > .5 & pred_probs[,5] <= .5 ) # 16 no longer gentrify

hist( pred_probs[,5] - pred_probs[,1] )



sum( pred_probsV[,1] > .5 ) # 87 tracts
sum( pred_probsV[,5] > .5 ) # 72 tracts
which(pred_probsV[,1] > .5 & pred_probsV[,5] <= .5) # 15 no longer vulnerable


# create outcome variables
data <- st_as_sf(rundata)
threshhold = 0.4

data$type1218_predicted <- "Not vulnerable"
data$type1218_predicted[ pred_probsV[,1] > threshhold ] <- "Vulnerable, did not gentrify"
data$type1218_predicted[ pred_probs[,1] > threshhold ] <- "Vulnerable, gentrified"

data$type1218_intervention <- "Not vulnerable"
data$type1218_intervention[ pred_probsV[,5] > threshhold ] <- "Vulnerable, did not gentrify"
data$type1218_intervention[ pred_probs[,5] > threshhold ] <- "Vulnerable, gentrified"

table(data$type1218)
table(data$type1218_predicted)
table(data$type1218_intervention)

#
# Choropleth plots: 10% housing price reduction ------------------------------------------------------
#

library(tigris)
library(ggplot2)
library(ggthemes)

ffxgeo <- tracts(state = 51, county = 059, year = 2018) 
ffxgeo <- st_as_sf(ffxgeo)
ffxgeo <- ffxgeo %>% select(GEOID, geometry)

p1 <- ggplot(data = data) +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(aes(fill = type1218), size = 0.2) +
  labs(title = "True Outcomes\nFairfax County Tract-Level Gentrification\n2008/12 to 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Status", guide = "legend", values = c("#FCFDBF", "#FEC98D", "#F1605D"), na.value = "FFFFFF")

p2 <- ggplot(data = data) +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(aes(fill = type1218_predicted), size = 0.2) +
  labs(title = "Model Predicted Outcomes\nFairfax County Tract-Level Gentrification\n2008/12 to 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Status", guide = "legend", values = c("#FCFDBF", "#FEC98D", "#F1605D"), na.value = "FFFFFF")

p3 <- ggplot(data = data) +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(aes(fill = type1218_intervention), size = 0.2) +
  labs(title = "Intervention, 10% Housing Cost Reduction\nFairfax County Tract-Level Gentrification\n2008/12 to 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Status", guide = "legend", values = c("#FCFDBF", "#FEC98D", "#F1605D"), na.value = "FFFFFF")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

pdf("~/git/fairfax/src/gentri/interventionExample.pdf",width=12,height=5)
multiplot(p1,p2,p3,cols=3)
dev.off()

#
# Model predictions: Housing price ------------------------------------------------------
#

newdata_houseprice <- with(rundata@data, 
                           data.frame(tct_multunit12 = mean(tct_multunit12),
                                      tct_vacant12 = mean(tct_vacant12),
                                      tct_newbuild18 = mean(tct_newbuild18),
                                      chg1218_tct_singfam = mean(chg1218_tct_singfam),
                                      chg1218_tct_renters = mean(chg1218_tct_renters),
                                      chg1218_tct_medhome_pct = seq(0, 80, by = 5),
                                      chg1218_tct_medrent_pct = mean(chg1218_tct_medrent_pct),
                                      chg1218_tct_housdens = mean(chg1218_tct_housdens),
                                      chg1218_tct_popgrowth = mean(chg1218_tct_popgrowth),
                                      tct_rentburd12 = mean(tct_rentburd12), 
                                      tct_diffhou12 = mean(tct_diffhou12), 
                                      tct_transit12 = mean(tct_transit12),
                                      tct_unemp12 = mean(tct_unemp12),
                                      tct_inpov12 = mean(tct_inpov12),
                                      chg1218_tct_withba = mean(chg1218_tct_withba),
                                      chg1218_tct_nonhispwh = mean(chg1218_tct_nonhispwh),
                                      chg1218_tct_nonfam = mean(chg1218_tct_nonfam),
                                      chg1218_tct_hhinc_pct = mean(chg1218_tct_hhinc_pct)
                           ))

newdata_houseprice$housepriceP <- predict(predmodel, newdata = newdata_houseprice, type = "response")



# Get SE
newdata_houseprice <- cbind(newdata_houseprice, predict(predmodel, newdata = newdata_houseprice, type = "link",
                                                        se = TRUE))
newdata_houseprice <- within(newdata_houseprice, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# Plot
ggplot(newdata_houseprice, aes(x = chg1218_tct_medhome_pct, y = PredictedProb)) + 
  labs(title = "Predicted probability of tract gentrification by percent increase in median property value", 
       x = "% increase in median property value",
       y = "Predicted probability") +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2) + 
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(0, 80, 10), limits = c(0, 80)) +
  scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0, 1))


#
# Model predictions: Unemployment ------------------------------------------------------
#

newdata_unempl <- with(rundata@data, 
                       data.frame(tct_multunit12 = mean(tct_multunit12),
                                  tct_vacant12 = mean(tct_vacant12),
                                  tct_newbuild18 = mean(tct_newbuild18),
                                  chg1218_tct_singfam = mean(chg1218_tct_singfam),
                                  chg1218_tct_renters = mean(chg1218_tct_renters),
                                  chg1218_tct_medhome_pct = mean(chg1218_tct_medhome_pct),
                                  chg1218_tct_medrent_pct = mean(chg1218_tct_medrent_pct),
                                  chg1218_tct_housdens = mean(chg1218_tct_housdens),
                                  chg1218_tct_popgrowth = mean(chg1218_tct_popgrowth),
                                  tct_rentburd12 = mean(tct_rentburd12), 
                                  tct_diffhou12 = mean(tct_diffhou12), 
                                  tct_transit12 = mean(tct_transit12),
                                  tct_unemp12 = seq(0, 15, by = 5),
                                  tct_inpov12 = mean(tct_inpov12),
                                  chg1218_tct_withba = mean(chg1218_tct_withba),
                                  chg1218_tct_nonhispwh = mean(chg1218_tct_nonhispwh),
                                  chg1218_tct_nonfam = mean(chg1218_tct_nonfam),
                                  chg1218_tct_hhinc_pct = mean(chg1218_tct_hhinc_pct)
                       ))

newdata_unempl$unemplP <- predict(predmodel, newdata = newdata_unempl, type = "response")

# Get SE
newdata_unempl <- cbind(newdata_unempl, predict(predmodel, newdata = newdata_unempl, type = "link",
                                                se = TRUE))
newdata_unempl <- within(newdata_unempl, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# Plot
ggplot(newdata_unempl, aes(x = tct_unemp12, y = PredictedProb)) + 
  labs(title = "Predicted probability of tract gentrification by percent unemployed", 
       x = "% unemployed",
       y = "Predicted probability") +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2) + 
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(0, 15, 5), limits = c(0, 15)) +
  scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0, 1))      


#
# Model predictions: With BA ------------------------------------------------------
#

newdata_withba <- with(rundata@data, 
                       data.frame(tct_multunit12 = mean(tct_multunit12),
                                  tct_vacant12 = mean(tct_vacant12),
                                  tct_newbuild18 = mean(tct_newbuild18),
                                  chg1218_tct_singfam = mean(chg1218_tct_singfam),
                                  chg1218_tct_renters = mean(chg1218_tct_renters),
                                  chg1218_tct_medhome_pct = mean(chg1218_tct_medhome_pct),
                                  chg1218_tct_medrent_pct = mean(chg1218_tct_medrent_pct),
                                  chg1218_tct_housdens = mean(chg1218_tct_housdens),
                                  chg1218_tct_popgrowth = mean(chg1218_tct_popgrowth),
                                  tct_rentburd12 = mean(tct_rentburd12), 
                                  tct_diffhou12 = mean(tct_diffhou12), 
                                  tct_transit12 = mean(tct_transit12),
                                  tct_unemp12 = mean(tct_unemp12),
                                  tct_inpov12 = mean(tct_inpov12),
                                  chg1218_tct_withba = seq(0, 25, by = 5),
                                  chg1218_tct_nonhispwh = mean(chg1218_tct_nonhispwh),
                                  chg1218_tct_nonfam = mean(chg1218_tct_nonfam),
                                  chg1218_tct_hhinc_pct = mean(chg1218_tct_hhinc_pct)
                       ))

newdata_withba$withbaP <- predict(predmodel, newdata = newdata_withba, type = "response")

# Get SE
newdata_withba <- cbind(newdata_withba, predict(predmodel, newdata = newdata_withba, type = "link",
                                                se = TRUE))
newdata_withba <- within(newdata_withba, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# Plot
ggplot(newdata_withba, aes(x = chg1218_tct_withba, y = PredictedProb)) + 
  labs(title = "Predicted probability of tract gentrification by percent change in population with BA", 
       x = "% change in population with BA",
       y = "Predicted probability") +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2) + 
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(0, 25, 5), limits = c(0, 25)) +
  scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0, 1))   


#
# Model predictions: Non Hispanic white ------------------------------------------------------
#

newdata_nonhispw <- with(rundata@data, 
                         data.frame(tct_multunit12 = mean(tct_multunit12),
                                    tct_vacant12 = mean(tct_vacant12),
                                    tct_newbuild18 = mean(tct_newbuild18),
                                    chg1218_tct_singfam = mean(chg1218_tct_singfam),
                                    chg1218_tct_renters = mean(chg1218_tct_renters),
                                    chg1218_tct_medhome_pct = mean(chg1218_tct_medhome_pct),
                                    chg1218_tct_medrent_pct = mean(chg1218_tct_medrent_pct),
                                    chg1218_tct_housdens = mean(chg1218_tct_housdens),
                                    chg1218_tct_popgrowth = mean(chg1218_tct_popgrowth),
                                    tct_rentburd12 = mean(tct_rentburd12), 
                                    tct_diffhou12 = mean(tct_diffhou12), 
                                    tct_transit12 = mean(tct_transit12),
                                    tct_unemp12 = mean(tct_unemp12),
                                    tct_inpov12 = mean(tct_inpov12),
                                    chg1218_tct_withba = mean(chg1218_tct_withba),
                                    chg1218_tct_nonhispwh = seq(0, 20, by = 5),
                                    chg1218_tct_nonfam = mean(chg1218_tct_nonfam),
                                    chg1218_tct_hhinc_pct = mean(chg1218_tct_hhinc_pct)
                         ))

newdata_nonhispw$nonhispP <- predict(predmodel, newdata = newdata_nonhispw, type = "response")

# Get SE
newdata_nonhispw <- cbind(newdata_nonhispw, predict(predmodel, newdata = newdata_nonhispw, type = "link",
                                                    se = TRUE))
newdata_nonhispw <- within(newdata_nonhispw, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# Plot
ggplot(newdata_nonhispw, aes(x = chg1218_tct_nonhispwh, y = PredictedProb)) + 
  labs(title = "Predicted probability of tract gentrification by percent change in non-Hispanic white population", 
       x = "% change in non-Hispanic white population",
       y = "Predicted probability") +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.2) + 
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(0, 20, 5), limits = c(0, 20)) +
  scale_y_continuous(breaks = seq(0, 1, 0.20), limits = c(0, 1))   

