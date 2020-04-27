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
rundata <- rundata %>% arrange(GEOID)

# Create sp object
rundata <- as(rundata, "Spatial")

# Create other objects
Wnb <- poly2nb(rundata) # turn data into a neighborhood (nb) object
W <- nb2mat(Wnb, style = "B") # create neighborhood matrix


# 
# Try MVS.CARleroux -----------------------------------------------------------------------------------------------
#

# Create outcome variable as matrix
# A formula for the covariate part of the model using the syntax of the lm() function. 
# Offsets can be included here using the offset() function. The response and the offset 
# (if included) should be matrices of dimension K*J, where K is the number of spatial units
# and J is the number of different variables (categories in the multinomial model). 
# The covariates should each be a K*1 vector, and different regression parameters 
# are estimated for each of the J variables.

outcome <- rundata@data %>% select(GEOID, type1218)
outcome <- outcome %>% mutate(c1_notvul = ifelse(type1218 == "Not vulnerable", 1, 0),
                              c2_vulnotg = ifelse(type1218 == "Vulnerable, did not gentrify", 1, 0),
                              c3_vulg = ifelse(type1218 == "Vulnerable, gentrified", 1, 0))
outcome <- outcome %>% select(c1_notvul, c2_vulnotg, c3_vulg)
outcome <- as.matrix(outcome)

# Create trials variable
modeltrials <- matrix(rep(100, 747), nrow = 249, ncol = 3, byrow = TRUE)

# Transform covariate to a vector to try in model
cov_tct_rentburd12 <- rundata@data %>% select(GEOID, tct_rentburd12) %>% arrange(GEOID) %>% select(-GEOID)
cov_tct_rentburd12 <- cov_tct_rentburd12$tct_rentburd12

# Try model ----------------------------------
chain1 <- MVS.CARleroux(formula = outcome ~ cov_tct_rentburd12,
                        family = "multinomial", W = W, burnin = 10, n.sample = 50, trials = modeltrials)
# Error in eval(family$initialize) : 
# for the 'quasibinomial' family, y must be a vector of 0 and 1's
# or a 2 column matrix where col 1 is no. successes and col 2 is no. failures

# Try with everything in one dataframe ---------------------------------
rundata@data <- rundata@data %>% mutate(c1_notvul = ifelse(type1218 == "Not vulnerable", 1, 0),
                                        c2_vulnotg = ifelse(type1218 == "Vulnerable, did not gentrify", 1, 0),
                                        c3_vulg = ifelse(type1218 == "Vulnerable, gentrified", 1, 0))
chain1 <- MVS.CARleroux(formula = as.matrix(rundata@data[ , c("c1_notvul", "c2_vulnotg", "c3_vulg")]) ~ tct_rentburd12, 
                        data = rundata@data, family = "multinomial", W = W, burnin = 10, n.sample = 50, trials = modeltrials)
# Error in eval(family$initialize) : 
# for the 'quasibinomial' family, y must be a vector of 0 and 1's
# or a 2 column matrix where col 1 is no. successes and col 2 is no. failures

