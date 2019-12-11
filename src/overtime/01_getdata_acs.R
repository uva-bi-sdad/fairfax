library(rgdal)
library(tidycensus)
library(tigris)
library(acs)
library(rgeos)
library(raster)
library(dplyr)
library(sf)
library(readr)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# Select variables -------------------------------------------------------------------------------------------------------------
#

# For datasets from 2008-12 to 2013-17
vars0913_1317 <- c(
  "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", 
  "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010",
  "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", # less than HS: (002 to 015)/001
  "B03003_001", "B03003_003",                                           # hispanic: 003/001
  "B02001_001", "B02001_003",                                           # black: 003/001
  "B17020_001", "B17020_002",                                           # in poverty: 002/001
  "B09005_001", "B09005_004", "B09005_005")                             # single parent: (004+005)/001

# For dataset 2008-12
vars0812 <- c(
  "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", 
  "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010",
  "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", # less than HS: (002 to 015)/001
  "B03003_001", "B03003_003",                                           # hispanic: 003/001
  "B02001_001", "B02001_003",                                           # black: 003/001
  "B17025_001", "B17025_002",                                           # in poverty: 002/001
  "B09005_001", "B09005_004", "B09005_005")                             # single parent: (004+005)/001

# For datasets 2005-09 to 2007-11
vars0509_0711 <- c(
  "B15002_001", "B15002_003", "B15002_004", "B15002_005", "B15002_006", 
  "B15002_007", "B15002_008", "B15002_009", "B15002_010", "B15002_020", 
  "B15002_021", "B15002_022", "B15002_023", "B15002_024", "B15002_025", 
  "B15002_026", "B15002_027",                                         # less than HS: (003 to 010 + 020 to 027)/001
  "B03003_001", "B03003_003",                                         # hispanic: 003/001
  "B02001_001", "B02001_003",                                         # black: 003/001
  "B17025_001", "B17025_002",                                         # in poverty: 002/001
  "B09005_001", "B09005_004", "B09005_005")                           # single parent: (004+005)/001


#
# Get data ------------------------------------------------------------------------------------------------------------
#

acs1317 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars0913_1317, year = 2017, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs1216 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars0913_1317, year = 2016, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs1115 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars0913_1317, year = 2015, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs1014 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars0913_1317, year = 2014, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs0913 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars0913_1317, year = 2013, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE) 

acs0812 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars0812, year = 2012, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs0711 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars0509_0711, year = 2011, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs0610 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars0509_0711, year = 2010, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs0509 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars0509_0711, year = 2009, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)


#
# Clean data ------------------------------------------------------------------------------------------------------------
#

# Variable set 1
acs1317 <- acs1317 %>% mutate(
  lesshs = (B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
    B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E) / B15003_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  inpoverty = B17020_002E / B17020_001E,
  singleparent = (B09005_004E + B09005_005E) / B09005_001E)

acs1216 <- acs1216 %>% mutate(
  lesshs = (B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
              B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E) / B15003_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  inpoverty = B17020_002E / B17020_001E,
  singleparent = (B09005_004E + B09005_005E) / B09005_001E)

acs1115 <- acs1115 %>% mutate(
  lesshs = (B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
              B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E) / B15003_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  inpoverty = B17020_002E / B17020_001E,
  singleparent = (B09005_004E + B09005_005E) / B09005_001E)

acs1014 <- acs1014 %>% mutate(
  lesshs = (B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
              B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E) / B15003_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  inpoverty = B17020_002E / B17020_001E,
  singleparent = (B09005_004E + B09005_005E) / B09005_001E)

acs0913 <- acs0913 %>% mutate(
  lesshs = (B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
              B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E) / B15003_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  inpoverty = B17020_002E / B17020_001E,
  singleparent = (B09005_004E + B09005_005E) / B09005_001E)

# Variable set 2
acs0812 <- acs0812 %>% mutate(
  lesshs = (B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
              B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E) / B15003_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  inpoverty = B17025_002E / B17025_001E,
  singleparent = (B09005_004E + B09005_005E) / B09005_001E)

# Variable set 3
acs0711 <- acs0711 %>% mutate(
  lesshs = (B15002_003E + B15002_004E + B15002_005E + B15002_006E + B15002_007E + B15002_008E + B15002_009E + B15002_010E +
            B15002_020E + B15002_021E + B15002_022E + B15002_023E + B15002_024E + B15002_025E + B15002_026E + B15002_027E) / B15002_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  inpoverty = B17025_002E / B17025_001E,
  singleparent = (B09005_004E + B09005_005E) / B09005_001E)

acs0610 <- acs0610 %>% mutate(
  lesshs = (B15002_003E + B15002_004E + B15002_005E + B15002_006E + B15002_007E + B15002_008E + B15002_009E + B15002_010E +
              B15002_020E + B15002_021E + B15002_022E + B15002_023E + B15002_024E + B15002_025E + B15002_026E + B15002_027E) / B15002_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  inpoverty = B17025_002E / B17025_001E,
  singleparent = (B09005_004E + B09005_005E) / B09005_001E)

acs0509 <- acs0509 %>% mutate(
  lesshs = (B15002_003E + B15002_004E + B15002_005E + B15002_006E + B15002_007E + B15002_008E + B15002_009E + B15002_010E +
              B15002_020E + B15002_021E + B15002_022E + B15002_023E + B15002_024E + B15002_025E + B15002_026E + B15002_027E) / B15002_001E,
  hispanic = B03003_003E / B03003_001E,
  black = B02001_003E / B02001_001E,
  inpoverty = B17025_002E / B17025_001E,
  singleparent = (B09005_004E + B09005_005E) / B09005_001E)