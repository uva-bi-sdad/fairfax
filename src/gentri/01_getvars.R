library(tidycensus)
library(acs)
library(dplyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(viridis)
library(scales)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# Select variables -------------------------------------------------------------------------------------------------------------
#

# 1) Vulnerability in the base year
# Exhibit 3+ of the following higher than county median: 
# B19013 for Higher percentage of low income households (80% of county median, HUD definition)
# B15003 for Higher percentage of 25+ without bachelor's
# B02001 for Higher percentage non-white
# B25003 for Higher percentage of renter households

# Sociodemographic change
# At least 1 change is greater than county change from base year:
# B15003 for Change in percent population 25+ with at least a BA, OR
# B19013 for Change in median HH income AND
# B03002 for Change in percent non Hispanic white

# Investment change
# At least 1 change is greater than county change:
# B25064 for Change in monthly median gross rent OR
# B25077 for Change in median home value

# Select variables
vars <- c(
  # median household income in past 12 months adjusted at end year
  "B19013_001", 
  # household income in past 12 months
  "B19001_001", "B19001_002", "B19001_003", "B19001_004", "B19001_005", "B19001_006", "B19001_007",
  "B19001_008", "B19001_009", "B19001_010", "B19001_011", "B19001_012", "B19001_013", "B19001_014",
  "B19001_015", "B19001_016", "B19001_017",
  # aged 25+ without and with BA+ (002-021 / 001 without, 022-025 / 001 with)
  "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006", "B15003_007", 
  "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", 
  "B15003_015", "B15003_016", "B15003_017", "B15003_018", "B15003_019", "B15003_020", "B15003_021", 
  "B15003_022", "B15003_023", "B15003_024", "B15003_025",
  # non-white (003 - 008 / 001)
  "B02001_001", "B02001_003", "B02001_004", "B02001_005", "B02001_006", "B02001_007", "B02001_008",
  # renters (003 / 001)
  "B25003_001", "B25003_003",
  # non-Hispanic white (003 / 001)
  "B03002_001", "B03002_003",
  # median gross rent
  "B25064_001", 
  # median home value
  "B25077_001"
  )                 


#
# Get variables for county medians --------------------------------------------------------------------------------------
#

acs1418cty <- get_acs(geography = "county", state = 51, county = 059, variables = vars, 
                   year = 2018, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = FALSE, keep_geo_vars = TRUE)

acs1317cty <- get_acs(geography = "county", state = 51, county = 059, variables = vars, 
                   year = 2017, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = FALSE, keep_geo_vars = TRUE)

acs1216cty <- get_acs(geography = "county", state = 51, county = 059, variables = vars, 
                   year = 2016, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = FALSE, keep_geo_vars = TRUE)

acs1115cty <- get_acs(geography = "county", state = 51, county = 059, variables = vars, 
                   year = 2015, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = FALSE, keep_geo_vars = TRUE)

acs1014cty <- get_acs(geography = "county", state = 51, county = 059, variables = vars, 
                   year = 2014, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = FALSE, keep_geo_vars = TRUE)

acs0913cty <- get_acs(geography = "county", state = 51, county = 059, variables = vars, 
                   year = 2013, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = FALSE, keep_geo_vars = TRUE)

acs0812cty <- get_acs(geography = "county", state = 51, county = 059, variables = vars, 
                   year = 2012, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = FALSE, keep_geo_vars = TRUE)


#
# Calculate county medians AND adjust to 2018 dollars ----------------------------------------------------------------------------------
#

# Constant dollar adjustment: Then-year estimate * (This-year CPI / Then-year CPI) 
# See https://www.census.gov/topics/income-poverty/income/guidance/current-vs-constant-dollars.html
# Adjusting median income, median gross rent, and median home price.
# I adjust from END year (rather than midpoint year) for consistency with HH income adjustments already in ACS.

acs1418cty <- acs1418cty %>% mutate(
              cty_hhinc18 = B19013_001E,
              cty_noba18 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + # 25+ without BA
                            B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                            B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                            B15003_020E + B15003_021E) / B15003_001E * 100,
              cty_nonwhite18 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E + # non white
                                B02001_008E) / B02001_001E * 100, 
              cty_renters18 = B25003_003E / B25003_001E * 100, # renters
              cty_withba18 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100, # 25+ with BA or +
              cty_medinc18 = B19013_001E, # median income
              cty_nonhispwh18 = B03002_003E / B03002_001E * 100, # nonHisp white
              cty_medrent18 = B25064_001E, # median gross rent
              cty_medhome18 = B25077_001E) %>% # median home value 
              select(GEOID, starts_with("cty_"))

acs1317cty <- acs1317cty %>% mutate(
              cty_hhinc17 = B19013_001E * (369.8 / 361),
              cty_noba17 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E +
                              B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                              B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                              B15003_020E + B15003_021E) / B15003_001E * 100,
              cty_nonwhite17 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E +
                                  B02001_008E) / B02001_001E * 100, 
              cty_renters17 = B25003_003E / B25003_001E * 100,
              cty_withba17 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100,
              cty_medinc17 = B19013_001E,
              cty_nonhispwh17 = B03002_003E / B03002_001E * 100,
              cty_medrent17 = B25064_001E * (369.8 / 361),
              cty_medhome17 = B25077_001E * (369.8 / 361)) %>%
              select(GEOID, starts_with("cty_"))

acs1216cty <- acs1216cty %>% mutate(
              cty_hhinc16 = B19013_001E * (369.8 / 353.4),
              cty_noba16 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E +
                              B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                              B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                              B15003_020E + B15003_021E) / B15003_001E * 100,
              cty_nonwhite16 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E +
                                  B02001_008E) / B02001_001E * 100, 
              cty_renters16 = B25003_003E / B25003_001E * 100,
              cty_withba16 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100,
              cty_medinc16 = B19013_001E,
              cty_nonhispwh16 = B03002_003E / B03002_001E * 100,
              cty_medrent16 = B25064_001E * (369.8 / 353.4),
              cty_medhome16 = B25077_001E * (369.8 / 353.4)) %>%
              select(GEOID, starts_with("cty_"))

acs1115cty <- acs1115cty %>% mutate(
              cty_hhinc15 = B19013_001E * (369.8 / 348.9),
              cty_noba15 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E +
                              B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                              B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                              B15003_020E + B15003_021E) / B15003_001E * 100,
              cty_nonwhite15 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E +
                                  B02001_008E) / B02001_001E * 100, 
              cty_renters15 = B25003_003E / B25003_001E * 100,
              cty_withba15 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100,
              cty_medinc15 = B19013_001E,
              cty_nonhispwh15 = B03002_003E / B03002_001E * 100,
              cty_medrent15 = B25064_001E * (369.8 / 348.9),
              cty_medhome15 = B25077_001E * (369.8 / 348.9)) %>%
              select(GEOID, starts_with("cty_"))

acs1014cty <- acs1014cty %>% mutate(
              cty_hhinc14 = B19013_001E * (369.8 / 348.3),
              cty_noba14 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E +
                              B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                              B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                              B15003_020E + B15003_021E) / B15003_001E * 100,
              cty_nonwhite14 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E +
                                  B02001_008E) / B02001_001E * 100, 
              cty_renters14 = B25003_003E / B25003_001E * 100,
              cty_withba14 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100,
              cty_medinc14 = B19013_001E,
              cty_nonhispwh14 = B03002_003E / B03002_001E * 100,
              cty_medrent14 = B25064_001E * (369.8 / 348.3),
              cty_medhome14 = B25077_001E * (369.8 / 348.3)) %>%
              select(GEOID, starts_with("cty_"))

acs0913cty <- acs0913cty %>% mutate(
              cty_hhinc13 = B19013_001E * (369.8 / 342.5),
              cty_noba13 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E +
                              B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                              B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                              B15003_020E + B15003_021E) / B15003_001E * 100,
              cty_nonwhite13 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E +
                                  B02001_008E) / B02001_001E * 100, 
              cty_renters13 = B25003_003E / B25003_001E * 100,
              cty_withba13 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100,
              cty_medinc13 = B19013_001E,
              cty_nonhispwh13 = B03002_003E / B03002_001E * 100,
              cty_medrent13 = B25064_001E * (369.8 / 342.5),
              cty_medhome13 = B25077_001E * (369.8 / 342.5)) %>%
              select(GEOID, starts_with("cty_"))

acs0812cty <- acs0812cty %>% mutate(
              cty_hhinc12 = B19013_001E * (369.8 / 337.5),
              cty_noba12 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E +
                              B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                              B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                              B15003_020E + B15003_021E) / B15003_001E * 100,
              cty_nonwhite12 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E +
                                  B02001_008E) / B02001_001E * 100, 
              cty_renters12 = B25003_003E / B25003_001E * 100,
              cty_withba12 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100,
              cty_medinc12 = B19013_001E,
              cty_nonhispwh12 = B03002_003E / B03002_001E * 100,
              cty_medrent12 = B25064_001E * (369.8 / 337.5),
              cty_medhome12 = B25077_001E * (369.8 / 337.5)) %>%
              select(GEOID, starts_with("cty_"))


#
# Get variables for tracts -------------------------------------------------------------------------------------------------------------
#

acs1418 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars, 
                   year = 2018, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs1317 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars, 
                   year = 2017, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs1216 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars, 
                   year = 2016, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs1115 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars, 
                   year = 2015, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs1014 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars, 
                   year = 2014, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs0913 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars, 
                   year = 2013, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

acs0812 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars, 
                   year = 2012, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)


#
# Calculate tract variables AND adjust to 2018 dollars ----------------------------------------------------------------------------
#

acs1418 <- acs1418 %>% mutate(
  tct_hhinc18 = B19013_001E,
  tct_noba18 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E +   # 25+ without BA
                  B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                  B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                  B15003_020E + B15003_021E) / B15003_001E * 100,
  tct_nonwhite18 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E +   # non white
                      B02001_008E) / B02001_001E * 100, 
  tct_renters18 = B25003_003E / B25003_001E * 100,   # renters
  tct_withba18 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100,   # 25+ with BA or +
  tct_medinc18 = B19013_001E,   # median income
  tct_nonhispwh18 = B03002_003E / B03002_001E * 100,   # nonHisp white
  tct_medrent18 = B25064_001E,   # median gross rent
  tct_medhome18 = B25077_001E) %>% # median home value 
  select(GEOID, NAME.y, geometry, starts_with("tct_"))

acs1317 <- acs1317 %>% mutate(
  tct_hhinc17 = B19013_001E * (369.8 / 361),
  tct_noba17 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + 
                  B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                  B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                  B15003_020E + B15003_021E) / B15003_001E * 100,
  tct_nonwhite17 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E + 
                      B02001_008E) / B02001_001E * 100, 
  tct_renters17 = B25003_003E / B25003_001E * 100, 
  tct_withba17 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100, 
  tct_medinc17 = B19013_001E,  
  tct_nonhispwh17 = B03002_003E / B03002_001E * 100,  
  tct_medrent17 = B25064_001E * (369.8 / 361),
  tct_medhome17 = B25077_001E * (369.8 / 361)) %>% 
  select(GEOID, NAME.y, geometry, starts_with("tct_"))

acs1216 <- acs1216 %>% mutate(
  tct_hhinc16 = B19013_001E * (369.8 / 353.4),
  tct_noba16 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + 
                  B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                  B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                  B15003_020E + B15003_021E) / B15003_001E * 100,
  tct_nonwhite16 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E + 
                      B02001_008E) / B02001_001E * 100, 
  tct_renters16 = B25003_003E / B25003_001E * 100, 
  tct_withba16 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100, 
  tct_medinc16 = B19013_001E,  
  tct_nonhispwh16 = B03002_003E / B03002_001E * 100,  
  tct_medrent16 = B25064_001E * (369.8 / 353.4),
  tct_medhome16 = B25077_001E * (369.8 / 353.4)) %>% 
  select(GEOID, NAME.y, geometry, starts_with("tct_"))

acs1115 <- acs1115 %>% mutate(
  tct_hhinc15 = B19013_001E * (369.8 / 348.9),
  tct_noba15 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + 
                  B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                  B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                  B15003_020E + B15003_021E) / B15003_001E * 100,
  tct_nonwhite15 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E + 
                      B02001_008E) / B02001_001E * 100, 
  tct_renters15 = B25003_003E / B25003_001E * 100, 
  tct_withba15 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100, 
  tct_medinc15 = B19013_001E,  
  tct_nonhispwh15 = B03002_003E / B03002_001E * 100,  
  tct_medrent15 = B25064_001E * (369.8 / 348.9),
  tct_medhome15 = B25077_001E * (369.8 / 348.9)) %>% 
  select(GEOID, NAME.y, geometry, starts_with("tct_"))

acs1014 <- acs1014 %>% mutate(
  tct_hhinc14 = B19013_001E * (369.8 / 348.3),
  tct_noba14 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + 
                  B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                  B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                  B15003_020E + B15003_021E) / B15003_001E * 100,
  tct_nonwhite14 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E + 
                      B02001_008E) / B02001_001E * 100, 
  tct_renters14 = B25003_003E / B25003_001E * 100, 
  tct_withba14 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100, 
  tct_medinc14 = B19013_001E,  
  tct_nonhispwh14 = B03002_003E / B03002_001E * 100,  
  tct_medrent14 = B25064_001E * (369.8 / 348.3),
  tct_medhome14 = B25077_001E * (369.8 / 348.3)) %>% 
  select(GEOID, NAME.y, geometry, starts_with("tct_"))

acs0913 <- acs0913 %>% mutate(
  tct_hhinc13 = B19013_001E * (369.8 / 342.5),
  tct_noba13 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + 
                  B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                  B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                  B15003_020E + B15003_021E) / B15003_001E * 100,
  tct_nonwhite13 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E + 
                      B02001_008E) / B02001_001E * 100, 
  tct_renters13 = B25003_003E / B25003_001E * 100, 
  tct_withba13 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100, 
  tct_medinc13 = B19013_001E,  
  tct_nonhispwh13 = B03002_003E / B03002_001E * 100,  
  tct_medrent13 = B25064_001E * (369.8 / 342.5),
  tct_medhome13 = B25077_001E * (369.8 / 342.5)) %>% 
  select(GEOID, NAME.y, geometry, starts_with("tct_"))

acs0812 <- acs0812 %>% mutate(
  tct_hhinc12 = B19013_001E * (369.8 / 337.5),
  tct_noba12 = (B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + 
                  B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E +
                  B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E + B15003_019E +
                  B15003_020E + B15003_021E) / B15003_001E * 100,
  tct_nonwhite12 = (B02001_003E + B02001_004E + B02001_005E + B02001_006E + B02001_007E + 
                      B02001_008E) / B02001_001E * 100, 
  tct_renters12 = B25003_003E / B25003_001E * 100, 
  tct_withba12 = (B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E * 100, 
  tct_medinc12 = B19013_001E,  
  tct_nonhispwh12 = B03002_003E / B03002_001E * 100,  
  tct_medrent12 = B25064_001E * (369.8 / 337.5),
  tct_medhome12 = B25077_001E * (369.8 / 337.5)) %>% 
  select(GEOID, NAME.y, geometry, starts_with("tct_"))


#
# Join over time -------------------------------------------------------------------------------------------------------------
#

# Tracts
ffxgeo <- acs1418 %>% select(GEOID, NAME.y, geometry)

acs1418 <- acs1418 %>% st_set_geometry(NULL)
acs1317 <- acs1317 %>% st_set_geometry(NULL)
acs1216 <- acs1216 %>% st_set_geometry(NULL)
acs1115 <- acs1115 %>% st_set_geometry(NULL)
acs1014 <- acs1014 %>% st_set_geometry(NULL)
acs0913 <- acs0913 %>% st_set_geometry(NULL)
acs0812 <- acs0812 %>% st_set_geometry(NULL)

ffx <- left_join(acs1418, acs1317, by = c("GEOID", "NAME.y"))
ffx <- left_join(ffx, acs1216, by = c("GEOID", "NAME.y"))
ffx <- left_join(ffx, acs1115, by = c("GEOID", "NAME.y"))
ffx <- left_join(ffx, acs1014, by = c("GEOID", "NAME.y"))
ffx <- left_join(ffx, acs0913, by = c("GEOID", "NAME.y"))
ffx <- left_join(ffx, acs0812, by = c("GEOID", "NAME.y"))
ffx <- left_join(ffx, ffxgeo, by = c("GEOID", "NAME.y"))

ffx <- st_as_sf(ffx)

# County
cty <- left_join(acs1418cty, acs1317cty)
cty <- left_join(cty, acs1216cty)
cty <- left_join(cty, acs1115cty)
cty <- left_join(cty, acs1014cty)
cty <- left_join(cty, acs0913cty)
cty <- left_join(cty, acs0812cty)


#
# Calculate -------------------------------------------------------------------------------------------------------------
#

# County-level change variables
cty <- cty %>% mutate(
                # change for one period, 18 - 12
                chg1218_cty_withba = cty_withba18 - cty_withba12,
                chg1218_cty_hhinc = cty_hhinc18 - cty_hhinc12,
                chg1218_cty_nonhispwh = cty_nonhispwh18 - cty_nonhispwh12,
                cgh1218_cty_medrent = cty_medrent18 - cty_medrent12,
                cgh1218_cty_medhome = cty_medhome18 - cty_medhome12,
                # change for two periods, 18-15, 15-12
                chg1215_cty_withba = cty_withba15 - cty_withba12,
                chg1215_cty_hhinc = cty_hhinc15 - cty_hhinc12,
                chg1215_cty_nonhispwh = cty_nonhispwh15 - cty_nonhispwh12,
                cgh1215_cty_medrent = cty_medrent15 - cty_medrent12,
                cgh1215_cty_medhome = cty_medhome15 - cty_medhome12,
                chg1518_cty_withba = cty_withba18 - cty_withba15,
                chg1518_cty_hhinc = cty_hhinc18 - cty_hhinc15,
                chg1518_cty_nonhispwh = cty_nonhispwh18 - cty_nonhispwh15,
                cgh1518_cty_medrent = cty_medrent18 - cty_medrent15,
                cgh1518_cty_medhome = cty_medhome18 - cty_medhome15)

# Tract-level change variables
ffx <- ffx %>% mutate(
                 # change for one period, 18 - 12
                chg1218_tct_withba = tct_withba18 - tct_withba12,
                chg1218_tct_hhinc = tct_hhinc18 - tct_hhinc12,
                chg1218_tct_nonhispwh = tct_nonhispwh18 - tct_nonhispwh12,
                cgh1218_tct_medrent = tct_medrent18 - tct_medrent12,
                cgh1218_tct_medhome = tct_medhome18 - tct_medhome12,
                # change for two periods, 18-15, 15-12
                chg1215_tct_withba = tct_withba15 - tct_withba12,
                chg1215_tct_hhinc = tct_hhinc15 - tct_hhinc12,
                chg1215_tct_nonhispwh = tct_nonhispwh15 - tct_nonhispwh12,
                cgh1215_tct_medrent = tct_medrent15 - tct_medrent12,
                cgh1215_tct_medhome = tct_medhome15 - tct_medhome12,
                chg1518_tct_withba = tct_withba18 - tct_withba15,
                chg1518_tct_hhinc = tct_hhinc18 - tct_hhinc15,
                chg1518_tct_nonhispwh = tct_nonhispwh18 - tct_nonhispwh15,
                cgh1518_tct_medrent = tct_medrent18 - tct_medrent15,
                cgh1518_tct_medhome = tct_medhome18 - tct_medhome15)

# Individual criterions
data <- ffx %>% transmute(GEOID = GEOID,
                          NAME.y = NAME.y, 
                          geometry = geometry,
                          # change 12-18
                              basehhinc = ifelse(tct_hhinc12 < acs0812cty$cty_hhinc12, 1, 0),
                              basenoba = ifelse(tct_noba12 > acs0812cty$cty_noba12, 1, 0),
                              basenonwhite = ifelse(tct_nonwhite12 > acs0812cty$cty_nonwhite12, 1, 0),
                              baserenters = ifelse(tct_renters12 > acs0812cty$cty_renters12, 1, 0),
                              chgba = ifelse(chg1218_tct_withba > cty$chg1218_cty_withba, 1, 0),
                              chghhinc = ifelse(chg1218_tct_hhinc > cty$chg1218_cty_hhinc, 1, 0),
                              chgnonhispwh = ifelse(chg1218_tct_nonhispwh > cty$chg1218_cty_nonhispwh, 1, 0),
                              chgmedrent = ifelse(cgh1218_tct_medrent > cty$cgh1218_cty_medrent, 1, 0),
                              chgmedhome = ifelse(cgh1218_tct_medhome > cty$cgh1218_cty_medhome, 1, 0),
                           # change 12-15
                              basehhinc1215 = ifelse(tct_hhinc12 < acs0812cty$cty_hhinc12, 1, 0),
                              basenoba1215 = ifelse(tct_noba12 > acs0812cty$cty_noba12, 1, 0),
                              basenonwhite1215 = ifelse(tct_nonwhite12 > acs0812cty$cty_nonwhite12, 1, 0),
                              baserenters1215 = ifelse(tct_renters12 > acs0812cty$cty_renters12, 1, 0),
                              chgba1215 = ifelse(chg1215_tct_withba > cty$chg1215_cty_withba, 1, 0),
                              chghhinc1215 = ifelse(chg1215_tct_hhinc > cty$chg1215_cty_hhinc, 1, 0),
                              chgnonhispwh1215 = ifelse(chg1215_tct_nonhispwh > cty$chg1215_cty_nonhispwh, 1, 0),
                              chgmedrent1215 = ifelse(cgh1215_tct_medrent > cty$cgh1215_cty_medrent, 1, 0),
                              chgmedhome1215 = ifelse(cgh1215_tct_medhome > cty$cgh1215_cty_medhome, 1, 0),
                           # change 15-18
                              basehhinc1518 = ifelse(tct_hhinc15 < acs0812cty$cty_hhinc12, 1, 0),
                              basenoba1518 = ifelse(tct_noba15 > acs0812cty$cty_noba12, 1, 0),
                              basenonwhite1518 = ifelse(tct_nonwhite15 > acs0812cty$cty_nonwhite12, 1, 0),
                              baserenters1518 = ifelse(tct_renters15 > acs0812cty$cty_renters12, 1, 0),
                              chgba1518 = ifelse(chg1518_tct_withba > cty$chg1518_cty_withba, 1, 0),
                              chghhinc1518 = ifelse(chg1518_tct_hhinc > cty$chg1518_cty_hhinc, 1, 0),
                              chgnonhispwh1518 = ifelse(chg1518_tct_nonhispwh > cty$chg1518_cty_nonhispwh, 1, 0),
                              chgmedrent1518 = ifelse(cgh1518_tct_medrent > cty$cgh1518_cty_medrent, 1, 0),
                              chgmedhome1518 = ifelse(cgh1518_tct_medhome > cty$cgh1518_cty_medhome, 1, 0))

# Group criteria
data <- data %>% mutate(# one period 12-18
                        meets_vuln1218 = ifelse(basehhinc + basenoba + basenonwhite + baserenters > 2, 1, 0),
                        meets_soc1218 = ifelse(chgba == 1 | (chghhinc == 1 & chgnonhispwh == 1), 1, 0),
                        meets_inv1218 = ifelse(chgmedrent == 1 | chgmedhome == 1, 1, 0),
                        gentrified1218 = ifelse(meets_vuln1218 == 1 & meets_soc1218 == 1 & meets_inv1218 == 1, 1, 0),
                        type1218 = case_when(meets_vuln1218 == 0 & gentrified1218 == 0 ~ "Not vulnerable",
                                             meets_vuln1218 == 1 & gentrified1218 == 0 ~ "Vulnerable, did not gentrify",
                                             meets_vuln1218 == 1 & gentrified1218 == 1 ~ "Vulnerable, gentrified"),
                        # two periods 12-15, 15-18
                        meets_vuln1215 = ifelse(basehhinc1215 + basenoba1215 + basenonwhite1215 + baserenters1215 > 2, 1, 0),
                        meets_soc1215 = ifelse(chgba1215 == 1 | (chghhinc1215 == 1 & chgnonhispwh1215 == 1), 1, 0),
                        meets_inv1215 = ifelse(chgmedrent1215 == 1 | chgmedhome1215 == 1, 1, 0),
                        gentrified1215 = ifelse(meets_vuln1215 == 1 & meets_soc1215 == 1 &  meets_inv1215 == 1, 1, 0),
                        type1215 = case_when(meets_vuln1215 == 0 & gentrified1215 == 0 ~ "Not vulnerable",
                                             meets_vuln1215 == 1 & gentrified1215 == 0 ~ "Vulnerable, did not gentrify",
                                             meets_vuln1215 == 1 & gentrified1215 == 1 ~ "Vulnerable, gentrified"),
                        meets_vuln1518 = ifelse(basehhinc1518 + basenoba1518 + basenonwhite1518 + baserenters1518 > 2, 1, 0),
                        meets_soc1518 = ifelse(chgba1518 == 1 | (chghhinc1518 == 1 & chgnonhispwh1518 == 1), 1, 0),
                        meets_inv1518 = ifelse(chgmedrent1518 == 1 | chgmedhome1518 == 1, 1, 0),
                        gentrified1518 = ifelse(meets_vuln1518 == 1 & meets_soc1518 == 1 &  meets_inv1518 == 1, 1, 0),
                        type1518 = case_when(meets_vuln1518 == 0 & gentrified1518 == 0 ~ "Not vulnerable",
                                             meets_vuln1518 == 1 & gentrified1518 == 0 ~ "Vulnerable, did not gentrify",
                                             meets_vuln1518 == 1 & gentrified1518 == 1 ~ "Vulnerable, gentrified"))

# Split dataframe
data_one <- data %>% select(GEOID, NAME.y, geometry,
                            meets_vuln1218, meets_soc1218, meets_inv1218,
                            basehhinc, basenoba, basenonwhite, baserenters, 
                            chgba, chghhinc, chgnonhispwh, chgmedrent, chgmedhome,
                            gentrified1218, type1218)
data_two <- data %>% select(GEOID, NAME.y, geometry,
                            meets_vuln1215, meets_soc1215, meets_inv1215,
                            meets_vuln1518, meets_soc1518, meets_inv1518,
                            basehhinc1215, basenoba1215, basenonwhite1215, baserenters1215, 
                            chgba1215, chghhinc1215, chgnonhispwh1215, chgmedrent1215, chgmedhome1215,
                            basehhinc1518, basenoba1518, basenonwhite1518, baserenters1518, 
                            chgba1518, chghhinc1518, chgnonhispwh1518, chgmedrent1518, chgmedhome1518,
                            gentrified1215, gentrified1518,
                            type1215, type1518)

  
# One time period
ggplot() +
  geom_sf(data = data_one, size = 0.2) +
  geom_sf(data = data[data$meets_vuln1218 == 1, ], aes(fill = "#440154")) +
  labs(title = "Fairfax County: Vulnerable") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1)) +
  scale_fill_identity(name = "Criterion", guide = "legend", labels = c("Vulnerable")) 

ggplot() +
  geom_sf(data = data_one, size = 0.2) +
  geom_sf(data = data[data$meets_soc1218 == 1, ], aes(fill = "#440154")) +
  labs(title = "Fairfax County: Socioeconomic change") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1)) +
  scale_fill_identity(name = "Criterion", guide = "legend", labels = c("Socioeconomic\nchange")) 

ggplot() +
  geom_sf(data = data_one, size = 0.2) +
  geom_sf(data = data[data$meets_inv1218 == 1, ], aes(fill = "#440154")) +
  labs(title = "Fairfax County: Investment change") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1)) +
  scale_fill_identity(name = "Criterion", guide = "legend", labels = c("Investment\nchange")) 

show_col(viridis_pal(option = "")(10))

ggplot(data = data_one) +
  geom_sf(aes(fill = type1218), size = 0.2) +
  labs(title = "Fairfax County Tract-Level Gentrification\n2008/12 to 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Status", guide = "legend", values = c("#FCFDBF", "#FEC98D", "#F1605D"), na.value = "FFFFFF")

# Two time periods
ggplot(data = data_two) +
  geom_sf(aes(fill = type1215), size = 0.2) +
  labs(title = "Fairfax County Tract-Level Gentrification\n2008/12 to 2011/15") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Status", guide = "legend", values = c("#FCFDBF", "#FEC98D", "#F1605D"), na.value = "FFFFFF")

ggplot(data = data_two) +
  geom_sf(aes(fill = type1518), size = 0.2) +
  labs(title = "Fairfax County Tract-Level Gentrification\n2011/15 to 2015/18") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Status", guide = "legend", values = c("#FCFDBF", "#FEC98D", "#F1605D"), na.value = "FFFFFF")


# DEAL WITH NAS!!!!