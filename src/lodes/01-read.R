library(readr)
library(dplyr)


# LODES documentation
# https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.4.pdf
# Using version 7.4. Version 7 of LODES was enumerated by 2010 census blocks. 

# WAC = workplace area characteristics, RAC = residence area characteristics, OD = origin-destination.
# OD – Origin-Destination data, jobs totals are associated with both a home Census Block and a work Census Block
# RAC – Residence Area Characteristic data, jobs are totaled by home Census Block
# WAC – Workplace Area Characteristic data, jobs are totaled by work Census Block

# Datasets for 2009-2017 contain additional variables (Race, Ethnicity, Education, and Sex) on the RAC and WAC files that are not available in other years of data.
# Firm Age and Firm Size variables are only available for data years 2011 and later, for All Private Jobs (JT02), and are made available through a Beta release. 
# Pulled data for "all jobs" and years 2009 and 2017 due to variable availability.
# Could pull for particular segments, other years.


#
# Read in data ---------------------------------------------------------
#

# Origin-destination
od09_main <- read_csv("./data/original/lodes/va_od_main_JT00_2009.csv", progress = show_progress(), col_names = TRUE, cols(w_geocode = "c", h_geocode = "c", createdate = "c", .default = col_double()))
od17_main <- read_csv("./data/original/lodes/va_od_main_JT00_2017.csv", progress = show_progress(), col_names = TRUE, cols(w_geocode = "c", h_geocode = "c", createdate = "c", .default = col_double()))
od09_aux <- read_csv("./data/original/lodes/va_od_aux_JT00_2009.csv", progress = show_progress(), col_names = TRUE, cols(w_geocode = "c", h_geocode = "c", createdate = "c", .default = col_double()))
od17_aux <- read_csv("./data/original/lodes/va_od_aux_JT00_2017.csv", progress = show_progress(), col_names = TRUE, cols(w_geocode = "c", h_geocode = "c", createdate = "c", .default = col_double()))

# Residence area characteristics
rac09 <- read_csv("./data/original/lodes/va_rac_S000_JT00_2009.csv", progress = show_progress(), col_names = TRUE, cols(h_geocode = "c", createdate = "c", .default = col_double()))
rac17 <- read_csv("./data/original/lodes/va_rac_S000_JT00_2017.csv", progress = show_progress(), col_names = TRUE, cols(h_geocode = "c", createdate = "c", .default = col_double()))

# Workplace area characteristics
wac09 <- read_csv("./data/original/lodes/va_wac_S000_JT00_2009.csv", progress = show_progress(), col_names = TRUE, cols(w_geocode = "c", createdate = "c", .default = col_double()))
wac17 <- read_csv("./data/original/lodes/va_wac_S000_JT00_2017.csv", progress = show_progress(), col_names = TRUE, cols(w_geocode = "c", createdate = "c", .default = col_double()))

# Geography xwalk
xwalk <- read_csv("./data/original/lodes/va_xwalk.csv", progress = show_progress(), col_names = TRUE, cols(blklatdd = "d", blklondd = "d", .default = col_character()))


#
# Filter to Fairfax County ---------------------------------------------------------
#

# Prepare geography xwalk (Fairfax County FIPS code = 51059)
xwalk_filt <- xwalk %>% select(tabblk2010, st, stusps, stname, cty, ctyname, trct, trctname, bgrp, bgrpname, blklatdd, blklondd)
xwalk_ffx <- xwalk_filt %>% filter(cty == "51059")











