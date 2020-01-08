
library(tidyverse)

setwd("~/Documents/FairfaxINOVA/Fairfax Datasets")
zip_tract <- read_csv("Zip_Tract_Data.csv")

zip_tract %>% 
  filter(zip == 22306 | zip == 22309 | # mount vernon 
           zip == 20190 | zip == 20191 | # reston
           zip == 20170 | zip == 20171 | # herndon 
           zip == 22041 | zip == 22044 | # crossroads area 
           zip == 22003 | zip == 22042 | zip == 22312 ) # annandale 
