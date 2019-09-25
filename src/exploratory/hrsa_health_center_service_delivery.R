library(readr)
library(stringr)
library(purrr)
library(dplyr)
library(sf)
library(maps)
library(ggplot2)


#
# Get data & filter ----------------------------------------------------------------------------------------------------
#

# Import
data <- read_csv("./data/original/hrsa/SITE_HCC_FCT_DET_fmt.csv", progress = show_progress())
head(data)

names(data)

# Fairfax, Virginia only
table(data$"State Name", useNA = "always")
virginia <- data %>% filter(`State Name` == "Virginia")

table(virginia$"Complete County Name", useNA = "always")
fairfax <- virginia %>% filter(`Complete County Name` == "Fairfax County")

names(fairfax)


#
# Check ----------------------------------------------------------------------------------------------------
#

table(fairfax$"Health Center Site Population Type Description") # 9 service delivery sites, 1 administrative
table(fairfax$"Health Center Service Delivery Site Location Setting Description") # all other clinic types
table(fairfax$"Site Status Description") # all active
table(fairfax$"Health Center Location Type Description") # all permanent
table(fairfax$"Health Center Type Description") # 9 service delivery sites, 1 administrative
table(fairfax$"Site Name")




