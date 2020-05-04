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

# Create trials variable (why 10? should be just 1 trial per tract)
modeltrials <- rep(1, nrow(cormatdata))

# Create outcome variable
rundata@data <- rundata@data %>% mutate(c1_notvul = ifelse(type1218 == "Not vulnerable", 1, 0),
                                        c2_vulnotg = ifelse(type1218 == "Vulnerable, did not gentrify", 1, 0),
                                        c3_vulg = ifelse(type1218 == "Vulnerable, gentrified", 1, 0))

rundata@data$c4_vul = rundata@data$c2_vulnotg + rundata@data$c3_vulg
# binomial outcomes: c3_vulg, c4_vul

# Model
# Inference for this model is based on 3 parallel Markov chains
# try running for 10 million samples, 1 million burn-in, thin every 2000 (5000 final samples)

# NOTE: with a binomial outcome we can use MALA=TRUE and get potentially much faster mixing (Langevin updates)
# the MCMC does do adaptive tuning as it runs, so no need to estimate the proposal s.d.

#data2 <- rundata@data %>% dplyr::select(tct_diffhou12,tct_newbuild18,tct_multunit12,tct_transit12,
#                                        chg1218_tct_renters,chg1218_tct_medhome_pct,chg1218_tct_medrent_pct,
#                                        chg1218_tct_singfam,chg1218_tct_popgrowth,chg1218_tct_housdens
#)
##cpairs(data2)
#cov.mean <- apply(data2,2,mean)
#cov.sd <- apply(data2,2,sd)
#for(j in 1:ncol(data2)){
#  data2[,j] <- (data2[,j]-cov.mean[j])/cov.sd[j]
#}

# try again with normalized betas (mean 0, sd 1)
#chain2.norm <- MVS.CARleroux(formula = as.matrix(rundata@data[ , c("c2_vulnotg", "c1_notvul", "c3_vulg")]) ~ 
#                          tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
#                          chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
#                          chg1218_tct_popgrowth + chg1218_tct_housdens,
#                        data = data2,
#                        family = "multinomial", W = W, burnin = 1e5, n.sample = 2.1e6, thin = 500, trials = modeltrials)

# outcome = Gentrified
chain1.binG <- S.CARleroux(formula = c3_vulg ~ 
                            tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
                            chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                            chg1218_tct_popgrowth + chg1218_tct_housdens,
                          data = rundata@data,
                          MALA = TRUE,
                          family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)

chain2.binG <- S.CARleroux(formula = c3_vulg ~ 
                            tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
                            chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                            chg1218_tct_popgrowth + chg1218_tct_housdens,
                          data = rundata@data,
                          MALA = TRUE,
                          family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)

chain3.binG <- S.CARleroux(formula = c3_vulg ~ 
                            tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
                            chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                            chg1218_tct_popgrowth + chg1218_tct_housdens,
                          data = rundata@data,
                          MALA = TRUE,
                          family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)


chain1.binV <- S.CARleroux(formula = c4_vul ~ 
                            tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
                            chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                            chg1218_tct_popgrowth + chg1218_tct_housdens,
                          data = rundata@data,
                          MALA = TRUE,
                          family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)

chain2.binV <- S.CARleroux(formula = c4_vul ~ 
                            tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
                            chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                            chg1218_tct_popgrowth + chg1218_tct_housdens,
                          data = rundata@data,
                          MALA = TRUE,
                          family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)

chain3.binV <- S.CARleroux(formula = c4_vul ~ 
                            tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
                            chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                            chg1218_tct_popgrowth + chg1218_tct_housdens,
                          data = rundata@data,
                          MALA = TRUE,
                          family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)




chain1 <- MVS.CARleroux(formula = as.matrix(rundata@data[ , c("c2_vulnotg", "c1_notvul", "c3_vulg")]) ~ 
                          tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
                          chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                          chg1218_tct_popgrowth + chg1218_tct_housdens,
                        data = rundata@data, 
                        family = "multinomial", W = W, burnin = 1e5, n.sample = 2.1e6, thin = 500, trials = modeltrials)
chain2 <- MVS.CARleroux(formula = as.matrix(rundata@data[ , c("c2_vulnotg", "c1_notvul", "c3_vulg")]) ~ 
                          tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
                          chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                          chg1218_tct_popgrowth + chg1218_tct_housdens,
                        data = rundata@data, 
                        family = "multinomial", W = W, burnin = 1e6, n.sample = 1.1e7, thin = 2000, trials = modeltrials)
chain3 <- MVS.CARleroux(formula = as.matrix(rundata@data[ , c("c2_vulnotg", "c1_notvul", "c3_vulg")]) ~ 
                          tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
                          chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
                          chg1218_tct_popgrowth + chg1218_tct_housdens,
                        data = rundata@data, 
                        family = "multinomial", W = W, burnin = 1e6, n.sample = 1.1e7, thin = 2000, trials = modeltrials)

save.image("~/git/fairfax/src/gentri/mcmcoutBinomial.RData")

# Variables I want, ideally
# tct_diffhou12 + tct_newbuild18 + tct_multunit12 + tct_transit12 + 
# chg1218_tct_renters + chg1218_tct_medhome_pct + chg1218_tct_medrent_pct + chg1218_tct_singfam + 
# chg1218_tct_popgrowth + chg1218_tct_housdens

# initially in the code:
# chg1218_tct_renters + chg1218_tct_medhome_pct + tct_newbuild18 + 
# chg1218_tct_medrent_pct + chg1218_tct_singfam + 
# chg1218_tct_housdens, data = rundata@data, 

# Output:
# (i) posterior median (Median); 
# (ii) 95% credible intervals (2.5%, 97.5%); 
# (iii) the effective number of independent samples (n.effective); 
# (iv) the convergence diagnostic proposed by Geweke (1992) (Geweke.diag) as a Z-score (should be <1.96)
print(chain1.bin)
print(chain2.bin)
print(chain3.bin)

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
chain1.bin$modelfit
chain2.bin$modelfit
chain3.bin$modelfit

# Assess MCMC sample convergence
beta.samplesG <- mcmc.list(chain1.binG$samples$beta, chain2.binG$samples$beta, chain3.binG$samples$beta)
beta.samplesV <- mcmc.list(chain1.binV$samples$beta, chain2.binV$samples$beta, chain3.binV$samples$beta)
# 2000 x 11 (intercept + 10 variables)

#plot(chain1$samples$beta[,13],type="l")

# plots of posterior distribution for each of the three chains
plot(beta.samplesG[,10])
plot(beta.samplesV[,10])

library(coda)
beta.samplesGall <- mcmc(rbind(chain1.binG$samples$beta, chain2.binG$samples$beta, chain3.binG$samples$beta))
beta.samplesVall <- mcmc(rbind(chain1.binV$samples$beta, chain2.binV$samples$beta, chain3.binV$samples$beta))
HPDinterval(beta.samplesGall)
HPDinterval(beta.samplesVall)


#rho.samples <- mcmc.list(chain1.bin$samples$rho, chain2.bin$samples$rho, chain3.bin$samples$rho)
#plot(rho.samples[,1]) # values >0 show the best fit has a significant spatial correlation



# Potential scale reduction factor
# Total value less than 1.1 is suggestive of convergence.
# For covariates pproximate convergence is diagnosed when the upper limit is close to 1.
# A factor of 1 means that between variance and within chain variance are equal, 
# larger values mean that there is still a notable difference between chains.
# The gelman plot shows  the development of the scale-reduction over time (chain steps).
gelman.diag(beta.samples)
gelman.plot(beta.samples) #these should not be going back up


# --------------------------------------------
# output
# --------------------------------------------

# trace plots of (beta.samples) for each of 10 variables (5x2) for the two models
# posterior means + HPD credible intervals for each of the 10 variables for the two models (tables)

# show ability to do model predictions under new covariates

# --------------------------------------------
# compare to nonspatial models
# --------------------------------------------

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

# --------------------------------------------
# trace plots for each model
# --------------------------------------------

pdf("~/git/fairfax/src/gentri/trace_binG.pdf",height=6,width=4)
par(mar=c(0,0,0,0))
plot(beta.samplesG[,1:11])
dev.off()

pdf("~/git/fairfax/src/gentri/trace_binV.pdf",height=6,width=4)
par(mar=c(0,0,0,0))
plot(beta.samplesV[,1:11])
dev.off()


# --------------------------------------------
# AUC, goodness of model fit for each model
# --------------------------------------------

library(pROC)

pdf("~/git/fairfax/src/gentri/auc.pdf",width=8,height=4)
par(mfrow=c(1,2))
plot( roc(response=rundata@data$c3_vulg, predictor=chain1.binG$fitted.values), main="Outcome:Gentrify" )
text(x=0.4,y=0.4,labels="AUC=0.83")

plot( roc(response=rundata@data$c4_vul, predictor=chain1.binV$fitted.values), main="Outcome:Vulnerable or Gentrify" )
text(x=0.4,y=0.4,labels="AUC=0.84")
dev.off()

# --------------------------------------------
# model prediction at new values of covariates
# --------------------------------------------

chain1.binG$fitted.values # model estimated probability of each tract to gentrify

# predict( chain1.binG ) # no 'predict' method for CARBayes; need to do it manually...




