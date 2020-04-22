library(CARBayes)
library(sp)
library(spdep)
library(nnet)

options(scipen = 999)

# For multinomial (no spatial effects): https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# For spatial effects: https://cran.r-project.org/web/packages/CARBayes/vignettes/CARBayes.pdf


#
# Prepare ------------------------------------------------------------------------------------------
#

# Create sp object
rundata <- as(alldata, "Spatial")


#
# Non-spatial model (multinomial logistic) ---------------------------------------------------------
#

# Select baseline outcome level
rundata$type1218 <- factor(rundata$type1218, 
                           levels = c("Not vulnerable", "Vulnerable, did not gentrify", "Vulnerable, gentrified"))
rundata$type1218 <- relevel(rundata$type1218, ref = "Not vulnerable")

# Model
# Output includes some iteration history and includes the final log-likelihood.
# This value multiplied by two is then seen in the model summary as the Residual Deviance 
# and it can be used in comparisons of nested models.
testmodel <- multinom(type1218 ~ chg1218_tct_popdens, data = rundata@data)
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

# Compute Moran’s I statistic
# To quantify the presence of spatial autocorrelation in the model residuals
# Conduct a permutation test to assess its significance. 
# The permutation test has the null hypothesis of no spatial autocorrelation and an alternative 
# hypothesis of positive spatial autocorrelation.
Wnb <- poly2nb(rundata, row.names = rundata@data$GEOID) # turn data into a neighborhood (nb) object
Wlist <- nb2listw(Wnb, style = "B") # turn data into a listw object, the required form of binary spatial adjacency information (based on border sharing) used by the moran.mc function
moran.mc(x = residuals(testmodel), listw = Wlist, nsim = 1000) # run spatial autocorrelation test

length(resid(testmodel)) 
length(Wlist)

### But it works for a logistic
rundata$type1218 <- fct_collapse(rundata$type1218,
             notgentrified = c("Not vulnerable", "Vulnerable, did not gentrify"),
             gentrified = "Vulnerable, gentrified")
             
testmodel <- glm(type1218 ~ chg1218_tct_popdens,
    data = rundata@data, family = binomial(link = "logit"))
