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
testmodel <- multinom(type1218 ~ chg1218_tct_popdens + chg1218_tct_housdens + chg1218_tct_renters, data = rundata)
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

