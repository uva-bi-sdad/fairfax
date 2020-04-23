library(tidycensus)
library(tigris)
library(acs)
library(dplyr)
library(sf)
library(naniar)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# Select tables ---------------------------------------------------------------------
#

acstables <- c(
  # gross rent as pct hh income
  "B25070",
  # geo mobility/residence
  "B07003",
  # occupancy status
  "B25002",
  # means of transportation to work
  "B08101",
  # household type for children
  "B09005",
  # household type
  "B11001",
  # employment status
  "B23025",
  # poverty status
  "B06012",
  # units in structure
  "B25024",
  # year moved in
  "B25026",
  # race and ethnicity
  "B03002",
  # tenure
  "B25003",
  # year built
  "B25034"
)


#
# Get variables ---------------------------------------------------------------------
#

# Get 2014/18
acsdata18 <- get_acs(geography = "tract", state = 51, county = 059, table = acstables[1], 
                   year = 2018, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
for(i in 2:length(acstables)){
  tmp <- get_acs(geography = "tract", state = 51, county = 059, table = acstables[i], 
                 year = 2018, survey = "acs5", cache_table = TRUE, 
                 output = "wide", geometry = FALSE)
  acsdata18 <- left_join(acsdata18, tmp)
}

remove(tmp)

# Get 2008/12
acsdata12 <- get_acs(geography = "tract", state = 51, county = 059, table = acstables[1], 
                     year = 2012, survey = "acs5", cache_table = TRUE, 
                     output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
for(i in 2:length(acstables)){
  tmp <- get_acs(geography = "tract", state = 51, county = 059, table = acstables[i], 
                 year = 2012, survey = "acs5", cache_table = TRUE, 
                 output = "wide", geometry = FALSE)
  acsdata12 <- left_join(acsdata12, tmp)
}

remove(tmp)

# Check variables
setdiff(names(acsdata12), names(acsdata18))

# Select estimates, drop MOEs for now
acsdata18 <- acsdata18 %>% select(1:10, NAME, ends_with("E"), geometry)
acsdata12 <- acsdata12 %>% select(1:13, NAME, ends_with("E"), geometry)


#
# Missingness ---------------------------------------------------------------------
#

# Missingness in 2018: 
# Nothing missing on these variables.
missvar18 <- miss_var_summary(acsdata18) 
miss_case_summary(acsdata18)
miss_case_table(acsdata18)

# Missingness in 2012:
# Nothing missing on these variables.
missvar12 <- miss_var_summary(acsdata12)
miss_case_summary(acsdata12)
miss_case_table(acsdata12)


#
# Calculate variables ---------------------------------------------------------------------
#

# Variables already calculated:
# tct_withba12          Percent Bachelor's or Higher (2000)
# tct_nonwhite12        Percent Nonwhite (2000)
# tct_hhinc12           Median Household Income (2000)
# tct_medhome12         Median Home Value (2000)
# tct_medrent12         Median Gross Rent (2000)
# chg1218_tct_withba    Change in Percent Bachelor's or Higher (00–16) 
# chg1218_tct_hhinc     Change in Median Household Income (00–16) 
# chg1218_tct_nonhispwh Change in Percent Non-Hispanic White (00–16)
# tct_renters12         Percent Renter Occupied (2000)
# chg1218_tct_medhome   Change in Median Home Value (00–16)
# chg1218_tct_renters   Change in percent renters

# Calculate
acsdata12 <- acsdata12 %>% mutate(
    tct_unemp12 = B23025_005E / B23025_002E * 100, # pct unemployed, unemployed / in labor force
    tct_inpov12 = B06012_002E / B06012_001E * 100, # pct below poverty, <100% poverty line / total  
    tct_multunit12 = (B25024_006E + B25024_007E + B25024_008E + B25024_009E) / B25024_001E * 100, # 5+ units in structure, (5 to 50+) / total
    tct_diffhou12 = (B07003_007E + B07003_010E + B07003_013E + B07003_016E) / B07003_001E * 100, # different house than 1yr ago , (all movers) / total      
    tct_nonfam12 = B11001_007E / B11001_001E * 100, # pct nonfamily hhs,            
    tct_singfam12 = (B25024_002E + B25024_003E) / B25024_001E * 100, # pct single family units, (attached+detached) / total 
    tct_vacant12 = B25002_003E / B25002_001E * 100, # pct vacant, vacant / total  
    tct_tradfam12 = B09005_003E / B09005_001E * 100, # pct married couple with children, in married family structure / total
    tct_transit12 = B08101_025E / B08101_001E * 100, # pct workers taking transit, public transport / total
    tct_rentburd12 = (B25070_008E + B25070_009E + B25070_010E) / B25070_001E * 100, # pct rent burdened, 35%+ of income in rent / total 
    tct_popdens12 = B03002_001E / (ALAND / 1000000), # people per land area (in square km)
    tct_housdens12 = B25024_001E / (ALAND / 1000000), # buildings per land area (in square km)
    tct_totalpop12 = B03002_001E # total population
    ) %>% select("STATEFP", "COUNTYFP", "TRACTCE", "GEOID", starts_with("tct")) %>% st_set_geometry(NULL)

acsdata18 <- acsdata18 %>% mutate(
  tct_singfam18 = (B25024_002E + B25024_003E) / B25024_001E * 100, # pct single family units, (attached+detached) / total 
  tct_nonfam18 = B11001_007E / B11001_001E * 100, # pct nonfamily hhs,      
  tct_popdens18 = B03002_001E / (ALAND / 1000000), # people per land area (in square km)
  tct_housdens18 = B25024_001E / (ALAND / 1000000), # buildings per land area (in square km)
  tct_newbuild18 = (B25034_002E + B25034_003E) / B25034_001E * 100, # built since 2010, since 2010 / total
  tct_totalpop18 = B03002_001E # total population
  ) %>% select("STATEFP", "COUNTYFP", "TRACTCE", "GEOID", starts_with("tct")) %>% st_set_geometry(NULL)


#
# Join and calculate remaining vars, then join to "data" from 01_gentrif.R ---------------------------------------------------------
#

# Join covars from 12 and 18
setdiff(acsdata12$GEOID, acsdata18$GEOID)
ffx_covars <- full_join(acsdata12, acsdata18, by = c("STATEFP", "COUNTYFP", "TRACTCE", "GEOID"))

# Calculate remaining
ffx_covars <- ffx_covars %>% mutate(
  chg1218_tct_singfam = tct_singfam18 - tct_singfam12,
  chg1218_tct_nonfam = tct_nonfam18 - tct_nonfam12,
  chg1218_tct_popdens = (tct_popdens18 * 100 / tct_popdens12) - 100,
  chg1218_tct_housdens = (tct_housdens18 * 100 / tct_housdens12) - 100,
  chg1218_tct_popgrowth = (tct_totalpop18 * 100 / tct_totalpop12) - 100 # pop growth rate
)

# Join with data from 01_gentrif.R
alldata <- left_join(data, ffx_covars, by = "GEOID")


