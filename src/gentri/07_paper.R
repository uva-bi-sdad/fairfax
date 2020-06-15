library(CARBayes)
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
modeltrials <- rep(1, nrow(rundata@data))

# Create outcome variable
rundata@data <- rundata@data %>% mutate(c1_notvul = ifelse(type1218 == "Not vulnerable", 1, 0),
                                        c2_vulnotg = ifelse(type1218 == "Vulnerable, did not gentrify", 1, 0),
                                        c3_vulg = ifelse(type1218 == "Vulnerable, gentrified", 1, 0))

rundata@data$c4_vul <- rundata@data$c2_vulnotg + rundata@data$c3_vulg
# binomial outcomes: c3_vulg, c4_vul

# Model
# Inference for this model is based on 3 parallel Markov chains
# try running for 10 million samples, 1 million burn-in, thin every 2000 (5000 final samples)

# NOTE: with a binomial outcome we can use MALA=TRUE and get potentially much faster mixing (Langevin updates)
# the MCMC does do adaptive tuning as it runs, so no need to estimate the proposal s.d.

# Set seed
set.seed(76978)

# Outcome = Vulnerable and gentrified
chain1.binG <- S.CARleroux(formula = c3_vulg ~ 
                             chg1218_tct_multunit + chg1218_tct_vacant +  tct_newbuild18 + 
                             chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                             chg1218_tct_renters + chg1218_tct_medrent_pct + 
                             chg1218_tct_housdens + 
                             chg1218_tct_rentburd + chg1218_tct_diffhou + 
                             chg1218_tct_transit + 
                             chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                             chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                             chg1218_tct_popgrowth,
                             data = rundata@data,
                             MALA = TRUE,
                             family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)

chain2.binG <- S.CARleroux(formula = c3_vulg ~ 
                             chg1218_tct_multunit + chg1218_tct_vacant +  tct_newbuild18 + 
                             chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                             chg1218_tct_renters + chg1218_tct_medrent_pct + 
                             chg1218_tct_housdens + 
                             chg1218_tct_rentburd + chg1218_tct_diffhou + 
                             chg1218_tct_transit + 
                             chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                             chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                             chg1218_tct_popgrowth,
                           data = rundata@data,
                           MALA = TRUE,
                           family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)

chain3.binG <- S.CARleroux(formula = c3_vulg ~ 
                             chg1218_tct_multunit + chg1218_tct_vacant +  tct_newbuild18 + 
                             chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                             chg1218_tct_renters + chg1218_tct_medrent_pct + 
                             chg1218_tct_housdens + 
                             chg1218_tct_rentburd + chg1218_tct_diffhou + 
                             chg1218_tct_transit + 
                             chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                             chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                             chg1218_tct_popgrowth,
                           data = rundata@data,
                           MALA = TRUE,
                           family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)

# Outcome = Vulnerable but not gentrified
chain1.binVng <- S.CARleroux(formula = c2_vulnotg ~ 
                             chg1218_tct_multunit + chg1218_tct_vacant +  tct_newbuild18 + 
                             chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                             chg1218_tct_renters + chg1218_tct_medrent_pct + 
                             chg1218_tct_housdens + 
                             chg1218_tct_rentburd + chg1218_tct_diffhou + 
                             chg1218_tct_transit + 
                             chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                             chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                             chg1218_tct_popgrowth,
                           data = rundata@data,
                           MALA = TRUE,
                           family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)

chain2.binVng <- S.CARleroux(formula = c2_vulnotg ~ 
                             chg1218_tct_multunit + chg1218_tct_vacant +  tct_newbuild18 + 
                             chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                             chg1218_tct_renters + chg1218_tct_medrent_pct + 
                             chg1218_tct_housdens + 
                             chg1218_tct_rentburd + chg1218_tct_diffhou + 
                             chg1218_tct_transit + 
                             chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                             chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                             chg1218_tct_popgrowth,
                           data = rundata@data,
                           MALA = TRUE,
                           family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)

chain3.binVng <- S.CARleroux(formula = c2_vulnotg ~ 
                             chg1218_tct_multunit + chg1218_tct_vacant +  tct_newbuild18 + 
                             chg1218_tct_singfam + chg1218_tct_medhome_pct + 
                             chg1218_tct_renters + chg1218_tct_medrent_pct + 
                             chg1218_tct_housdens + 
                             chg1218_tct_rentburd + chg1218_tct_diffhou + 
                             chg1218_tct_transit + 
                             chg1218_tct_unemp + chg1218_tct_inpov + chg1218_tct_hhinc_pct + 
                             chg1218_tct_nonfam + chg1218_tct_withba + chg1218_tct_nonhispwh + 
                             chg1218_tct_popgrowth,
                           data = rundata@data,
                           MALA = TRUE,
                           family = "binomial", W = W, burnin = 1e4, n.sample = 2.1e5, thin = 100, trials = modeltrials)

# Save models
saveRDS(chain1.binG, file = "./rivanna_data/working/paper/chain1.binG.Rds")
saveRDS(chain2.binG, file = "./rivanna_data/working/paper/chain2.binG.Rds")
saveRDS(chain3.binG, file = "./rivanna_data/working/paper/chain3.binG.Rds")

saveRDS(chain1.binVng, file = "./rivanna_data/working/paper/chain1.binVng.Rds")
saveRDS(chain2.binVng, file = "./rivanna_data/working/paper/chain2.binVng.Rds")
saveRDS(chain3.binVng, file = "./rivanna_data/working/paper/chain3.binVng.Rds")


# Output:
# (i) posterior median (Median); 
# (ii) 95% credible intervals (2.5%, 97.5%); 
# (iii) the effective number of independent samples (n.effective); 
# (iv) the convergence diagnostic proposed by Geweke (1992) (Geweke.diag) as a Z-score (should be <1.96)
print(chain1.binG)
print(chain2.binG)
print(chain3.binG)

print(chain1.binVng)
print(chain2.binVng)
print(chain3.binVng)

# Combine results: Vulnerable and gentrified
beta.samples.matrixG <- rbind(chain1.binG$samples$beta, chain1.binG$samples$beta, chain1.binG$samples$beta)
colnames(beta.samples.matrixG) <- colnames(chain1.binG$X)
# Then posterior medians and 95% credible intervals can be computed as follows:
round(t(apply(beta.samples.matrixG, 2, quantile, c(0.5, 0.025, 0.975))),5)

# Combine results: Vulnerable but not gentrified
beta.samples.matrixVng <- rbind(chain1.binVng$samples$beta, chain1.binVng$samples$beta, chain1.binVng$samples$beta)
colnames(beta.samples.matrixVng) <- colnames(chain1.binVng$X)
# Then posterior medians and 95% credible intervals can be computed as follows:
round(t(apply(beta.samples.matrixVng, 2, quantile, c(0.5, 0.025, 0.975))),5)


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
chain1.binG$modelfit
chain2.binG$modelfit
chain3.binG$modelfit

chain1.binVng$modelfit
chain2.binVng$modelfit
chain3.binVng$modelfit

# Assess MCMC sample convergence
beta.samplesG <- mcmc.list(chain1.binG$samples$beta, chain2.binG$samples$beta, chain3.binG$samples$beta)
beta.samplesVng <- mcmc.list(chain1.binVng$samples$beta, chain2.binVng$samples$beta, chain3.binVng$samples$beta)
# 2000 x 19 (intercept + 18 variables)

#plot(chain1$samples$beta[,13],type="l")

# plots of posterior distribution for each of the three chains
plot(beta.samplesG[,18])
plot(beta.samplesVng[,18])

library(coda)
beta.samplesGall <- mcmc(rbind(chain1.binG$samples$beta, chain2.binG$samples$beta, chain3.binG$samples$beta))
beta.samplesVall <- mcmc(rbind(chain1.binVng$samples$beta, chain2.binVng$samples$beta, chain3.binVng$samples$beta))
HPDinterval(beta.samplesGall)
HPDinterval(beta.samplesVall)

# Rho
rho.samplesG <- mcmc.list(chain1.binG$samples$rho, chain2.binG$samples$rho, chain3.binG$samples$rho)
rho.samplesVng <- mcmc.list(chain1.binVng$samples$rho, chain2.binVng$samples$rho, chain3.binVng$samples$rho)
plot(rho.samplesG[, 1]) # values >0 show the best fit has a significant spatial correlation
plot(rho.samplesVng[, 1])

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

