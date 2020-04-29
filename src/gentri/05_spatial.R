library(CARBayes)
library(sp)
library(spdep)
library(nnet)
library(MASS)
library(coda)
library(readr)
library(dplyr)
library(gclus)

options(scipen = 999)

# For multinomial (no spatial effects): https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# For spatial effects: https://cran.r-project.org/web/packages/CARBayes/vignettes/CARBayes.pdf

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
# Try MVS.CARleroux -----------------------------------------------------------------------------------------------
#

# Create trials variable
modeltrials <- rep(10, 249)

# Create outcome variable
rundata@data <- rundata@data %>% mutate(c1_notvul = ifelse(type1218 == "Not vulnerable", 1, 0),
                                        c2_vulnotg = ifelse(type1218 == "Vulnerable, did not gentrify", 1, 0),
                                        c3_vulg = ifelse(type1218 == "Vulnerable, gentrified", 1, 0))

# Model
# Inference for this model is based on 3 parallel Markov chains, each of which has been run 
# for 300,000 samples, the first 100,000 of which have been removed as the burn-in period. 
# The remaining 200,000 samples are thinned by 100 to reduce their temporal autocorrelation,
# resulting in 6,000 samples for inference across the 3 Markov chains.
# Note: run time is ~14 minutes each chain at burnin = 100.000 n.sample = 300.000, and ~2 minutes at divided by 10
chain1 <- MVS.CARleroux(formula = as.matrix(rundata@data[ , c("c2_vulnotg", "c1_notvul", "c3_vulg")]) ~ 
                          chg1218_tct_renters + chg1218_tct_medhome_pct + tct_newbuild18 +
                          chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                          chg1218_tct_housdens, data = rundata@data, 
                        family = "multinomial", W = W, burnin = 100000, n.sample = 550000, thin = 5, trials = modeltrials)
chain2 <- MVS.CARleroux(formula = as.matrix(rundata@data[ , c("c2_vulnotg", "c1_notvul", "c3_vulg")]) ~ 
                          chg1218_tct_renters + chg1218_tct_medhome_pct + tct_newbuild18 + 
                          chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                          chg1218_tct_housdens, data = rundata@data, 
                        family = "multinomial", W = W, burnin = 100000, n.sample = 550000, thin = 5, trials = modeltrials)
chain3 <- MVS.CARleroux(formula = as.matrix(rundata@data[ , c("c2_vulnotg", "c1_notvul", "c3_vulg")]) ~ 
                          chg1218_tct_renters + chg1218_tct_medhome_pct + tct_newbuild18 + 
                          chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                          chg1218_tct_housdens, data = rundata@data, 
                        family = "multinomial", W = W, burnin = 100000, n.sample = 550000, thin = 5, trials = modeltrials)

# Variables I want, ideally
# tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
# chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
# chg1218_tct_popgrowth + chg1218_tct_housdens

# Output:
# (i) posterior median (Median); 
# (ii) 95% credible intervals (2.5%, 97.5%); 
# (iii) the effective number of independent samples (n.effective); 
# (iv) the convergence diagnostic proposed by Geweke (1992) (Geweke.diag) as a Z-score (should be <1)
print(chain1)
print(chain2)
print(chain3)

summary(chain1$samples)
summary(chain2$samples)
summary(chain3$samples)


# 
# Model diagnostics -----------------------------------------------------------------------------------------------
#

# http://www.math.kit.edu/stoch/lehre/abib2010w/media/coda.pdf
# http://sbfnk.github.io/mfiidd/slides/mcmc_slides2.pdf
# http://sbfnk.github.io/mfiidd/mcmc_diagnostics.html
# https://theoreticalecology.wordpress.com/2011/12/09/mcmc-chain-analysis-and-convergence-diagnostics-with-coda-in-r/
# http://patricklam.org/teaching/convergence_print.pdf
# http://www.johnmyleswhite.com/notebook/2010/08/29/mcmc-diagnostics-in-r-with-the-coda-package/
# http://wlm.userweb.mwn.de/R/wlmRcoda.htm

# Fit indices
chain1$modelfit
chain2$modelfit
chain3$modelfit

# Assess MCMC sample convergence
beta.samples <- mcmc.list(chain1$samples$beta, chain2$samples$beta, chain3$samples$beta)
plot(beta.samples[ , 2:4])

# Potential scale reduction factor
# Total value less than 1.1 is suggestive of convergence.
# For covariates pproximate convergence is diagnosed when the upper limit is close to 1.
# A factor of 1 means that between variance and within chain variance are equal, 
# larger values mean that there is still a notable difference between chains.
# The gelman plot shows  the development of the scale-reduction over time (chain steps).
gelman.diag(beta.samples)
gelman.plot(beta.samples) #these should not be going back up



