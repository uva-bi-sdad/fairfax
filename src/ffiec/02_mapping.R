rm(list = ls())
setwd("/project/biocomplexity/sdad/projects_data/project_data/ffx/fairfax-2019/original/ffiec/hmda-va-2007-17")
for (pkg in c("tidyverse", "data.table", "sf")) {library(pkg, character.only = TRUE)}

library(readr)
library(dplyr)
library(janitor)
library(tigris)
library(sf)
library(ggthemes)
library(ggplot2)
library(purrr)
library(viridis)
library(magick)


ffiec_data <- read_csv("cfbp-hmda-ffx-2007-17.csv")

ffiec_data %>% count(county_name)

ffiec_data <- ffiec_data %>% 
  rename(year = as_of_year) %>% 
  select(year, 
         #applicant demographics & co-applicant demographics
         owner_occupancy_name, applicant_ethnicity_name, applicant_race_name_1, applicant_sex_name, applicant_income_000s,
         co_applicant_ethnicity, co_applicant_race_name_1, co_applicant_sex_name, 
         
         # tract information 
         census_tract_number, population, minority_population, 
         number_of_owner_occupied_units, number_of_1_to_4_family_units, 
         hud_median_family_income, tract_to_msamd_income,
         
         #property and loan info
         property_type_name, loan_purpose_name, loan_amount_000s, 
         rate_spread, hoepa_status_name, lien_status_name,  
         
         # denial reasons 
         denial_reason_name_1, denial_reason_name_2, denial_reason_name_3
         )

setwd("/project/biocomplexity/sdad/projects_data/project_data/ffx/fairfax-2019/original/ffiec/ffiec-hmda")
lar_data_2017 <- read_csv("2017_public_lar.csv")


setwd("/project/biocomplexity/sdad/projects_data/project_data/ffx/fairfax-2019/original/ffiec/ffiec-hmda")
lar_data_2018 <- read_csv("2018_public_lar.csv")

setwd("/project/biocomplexity/sdad/projects_data/project_data/ffx/fairfax-2019/original/ffiec/cfpb-complaint-data")
complaints_data <- read_csv("complaints-va.csv")

setwd("/project/biocomplexity/sdad/projects_data/project_data/ffx/fairfax-2019/original/ffiec/cfpb-complaint-data")
complaints_data <- readxl("complaints-va.xlsx")














