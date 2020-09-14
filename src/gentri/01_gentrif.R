library(tidycensus)
library(tigris)
library(acs)
library(dplyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(viridis)
library(scales)
library(naniar)

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

acs0812cty <- get_acs(geography = "county", state = 51, county = 059, variables = vars, 
                      year = 2012, survey = "acs5", cache_table = TRUE, 
                      output = "wide", geometry = FALSE, keep_geo_vars = TRUE)

#
# Missingness for data I need right now -------------------
#

# ACS 14-18 county 
miss_var_summary(acs1418cty)
# B02001_001M all missing but these are margins of error, estimates are fine
# B03002_001M all missing but these are margins of error, estimates are fine

# ACS 08-12 county
miss_var_summary(acs0812cty)
# B02001_001M all missing but these are margins of error, estimates are fine
# B03002_001M all missing but these are margins of error, estimates are fine


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

acs0812 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars, 
                   year = 2012, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)


#
# Missingness for data I need right now ---------------------------------------------------------
#

# ACS 14-18 tract (var, cases, percent)
miss_var_summary(acs1418)
# 1 B25064_001M     20     7.75 --> these are margins of error, not needed
# 2 B25064_001E     15     5.81 --> MEDIAN GROSS RENT
# 3 B25077_001E      8     3.10 --> MEDIAN HOME VALUE
# 4 B25077_001M      8     3.10 --> these are margins of error, not needed
# 5 B19013_001M      5     1.94 --> these are margins of error, not needed
# 6 B19013_001E      3     1.16 --> MEDIAN HOUSEHOLD INCOME

# ACS 08-12 tract (var, cases, percent)
miss_var_summary(acs0812)
# 1 B25064_001M    104     40.3 --> these are margins of error, not needed
# 2 B25077_001M     12     4.65 --> these are margins of error, not needed
# 3 B25077_001E      7     2.71 --> MEDIAN HOME VALUE
# 4 B25064_001E      6     2.33 --> MEDIAN GROSS RENT
# 5 B19013_001M      4     1.55 --> these are margins of error, not needed
# 6 B19013_001E      3     1.16 --> MEDIAN HOUSEHOLD INCOME


#
# From 02_fillin.R file, replace NAs with latest available value  ---------------------------------------------------------------------
#

# 02_fillin.R finds the latest available value, assigned here to NAs in median house price,
# median gross rent, and median household income. 
# Backhouse/rent and fronthouse/rent variables are assigned values that equal the latest available year 
# from which the value is pulled. 

# Missingness in 2018
# Rent: 15 missing, 10 have values in previous years (3 in 2017, 2 in 2016, 1 in 2015, 3 in 2014, 1 in 2013)
# House 8 missing, 1 has value in previous years (1 in 2017)
# Income: 3 missing, none have values in previous years

# Missingness in 2012
# Rent: 6 missing, 1 has value in previous years (1 in 2011)
# House: 7 missing, 1 has value in previous years (1 in 2010)
# Income: 3 missing, none have values in previous years

# Reasons:
# https://www2.census.gov/programs-surveys/acs/tech_docs/data_suppression/ACSO_Data_Suppression.pdf
# “For detailed tables, a table is filtered out if the median coefficient of variation (CV) is greater than 0.61.
# ACS suppresses medians when the margins of error associated with the medians was larger than the median itself.
# (That median is statistically unreliable.)

# Fill in house for 2012
acs0812$backhouse <- NA
acs0812[acs0812$GEOID == "51059421900", ]$backhouse <- 2010
acs0812[acs0812$GEOID == "51059421900", ]$B25077_001E <- 350000

# Fill in rent for 2012
acs0812$backrent <- NA
acs0812[acs0812$GEOID == "51059491101", ]$backrent <- 2011
acs0812[acs0812$GEOID == "51059491101", ]$B25064_001E <- 2001

# Fill in house for 2018
acs1418$fronthouse <- NA
acs1418[acs1418$GEOID == "51059451602", ]$fronthouse <- 2017
acs1418[acs1418$GEOID == "51059451602", ]$B25077_001E <- 233600

# Fill in rent for 2018
acs1418$frontrent <- NA
acs1418[acs1418$GEOID == "51059415600", ]$frontrent <- 2013
acs1418[acs1418$GEOID == "51059415600", ]$B25064_001E <- 2001

acs1418[acs1418$GEOID == "51059481600", ]$frontrent <- 2012
acs1418[acs1418$GEOID == "51059481600", ]$B25064_001E <- 2001

acs1418[acs1418$GEOID == "51059450800", ]$frontrent <- 2017
acs1418[acs1418$GEOID == "51059450800", ]$B25064_001E <- 1462

acs1418[acs1418$GEOID == "51059482400", ]$frontrent <- 2014
acs1418[acs1418$GEOID == "51059482400", ]$B25064_001E <- 2001

acs1418[acs1418$GEOID == "51059480300", ]$frontrent <- 2014
acs1418[acs1418$GEOID == "51059480300", ]$B25064_001E <- 2001

acs1418[acs1418$GEOID == "51059492100", ]$frontrent <- 2015
acs1418[acs1418$GEOID == "51059492100", ]$B25064_001E <- 3501

acs1418[acs1418$GEOID == "51059480201", ]$frontrent <- 2016
acs1418[acs1418$GEOID == "51059480201", ]$B25064_001E <- 3501

acs1418[acs1418$GEOID == "51059480501", ]$frontrent <- 2017
acs1418[acs1418$GEOID == "51059480501", ]$B25064_001E <- 2776

acs1418[acs1418$GEOID == "51059480503", ]$frontrent <- 2016
acs1418[acs1418$GEOID == "51059480503", ]$B25064_001E <- 1935

acs1418[acs1418$GEOID == "51059460100", ]$frontrent <- 2017
acs1418[acs1418$GEOID == "51059460100", ]$B25064_001E <- 3501

acs1418[acs1418$GEOID == "51059491502", ]$frontrent <- 2014
acs1418[acs1418$GEOID == "51059491502", ]$B25064_001E <- 2001

# Check for completeness again (estimates only)
# ACS 04-18 tract (var, cases, percent)
miss_var_summary(acs1418)
# 5 B25077_001E      7     2.71
# 7 B25064_001E      4     1.55
# 8 B19013_001E      3     1.16

# ACS 08-12 tract (var, cases, percent)
miss_var_summary(acs0812)
# 5 B25077_001E      6     2.33
# 6 B25064_001E      5     1.94
# 8 B19013_001E      3     1.16

# Which tracts have missing data? 
test1418 <- acs1418 %>% filter(is.na(B25064_001E) | is.na(B25077_001E) | is.na(B19013_001E)) # 8 tracts
test0812 <- acs0812 %>% filter(is.na(B25064_001E) | is.na(B25077_001E) | is.na(B19013_001E)) # 8 tracts

setdiff(test1418$GEOID, test0812$GEOID)
# 2018: "51059421900" "51059416200" "51059491000" "51059461902" "51059491202" "51059980100" "51059980200" "51059980300"
# 2012: "51059492201" "51059416200" "51059491000" "51059461902" "51059491202" "51059980100" "51059980200" "51059980300"

# Missing on: "51059421900" "51059492201" "51059416200" "51059491000" "51059461902" "51059491202" "51059980100" "51059980200" "51059980300"
    

#
# Select complete cases ----------------------------------------------------------------------------
#

acs1418 <- acs1418 %>% filter(!is.na(B25064_001E) & !is.na(B25077_001E) & !is.na(B19013_001E))
acs0812 <- acs0812 %>% filter(!is.na(B25064_001E) & !is.na(B25077_001E) & !is.na(B19013_001E))


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
  tct_medinc12 = B19013_001E * (369.8 / 337.5),
  tct_nonhispwh12 = B03002_003E / B03002_001E * 100,  
  tct_medrent12 = B25064_001E * (369.8 / 337.5),
  tct_medhome12 = B25077_001E * (369.8 / 337.5)) %>% 
  select(GEOID, NAME.y, geometry, starts_with("tct_"))


#
# Join over time -------------------------------------------------------------------------------------------------------------
#

# Tracts
ffxgeo <- tracts(state = 51, county = 059, year = 2018) 
ffxgeo <- st_as_sf(ffxgeo)
ffxgeo <- ffxgeo %>% select(GEOID, geometry)

acs1418 <- acs1418 %>% st_set_geometry(NULL)
acs0812 <- acs0812 %>% st_set_geometry(NULL)

ffx <- full_join(acs1418, acs0812, by = c("GEOID", "NAME.y"))
ffx <- left_join(ffxgeo, ffx, by = c("GEOID"))
ffx <- st_as_sf(ffx)

# County
cty <- full_join(acs1418cty, acs0812cty)

# Note: Missingness is on that one tract that isn't missing from both time periods. Can filter out.


#
# Calculate -------------------------------------------------------------------------------------------------------------
#

# County-level change variables
cty <- cty %>% mutate(
  chg1218_cty_withba = cty_withba18 - cty_withba12,
  chg1218_cty_hhinc = cty_hhinc18 - cty_hhinc12,
  chg1218_cty_hhinc_pct = (cty_hhinc18 * 100 / cty_hhinc12) - 100,
  chg1218_cty_nonhispwh = cty_nonhispwh18 - cty_nonhispwh12,
  chg1218_cty_medrent = cty_medrent18 - cty_medrent12,
  chg1218_cty_medrent_pct = (cty_medrent18 * 100 / cty_medrent12) - 100,
  chg1218_cty_medhome = cty_medhome18 - cty_medhome12,
  chg1218_cty_medhome_pct = (cty_medhome18 * 100 / cty_medhome12) - 100)

# Filter out if info is unavailable
ffx <- ffx %>% filter(!is.na(tct_hhinc18) & !is.na(tct_hhinc12) &
                      !is.na(tct_medrent18) & !is.na(tct_medrent12) &
                      !is.na(tct_medhome18) & !is.na(tct_medhome12))

# Tract-level change variables
ffx <- ffx %>% mutate(
  chg1218_tct_withba = tct_withba18 - tct_withba12,
  chg1218_tct_hhinc = tct_hhinc18 - tct_hhinc12, 
  chg1218_tct_hhinc_pct = (tct_hhinc18 * 100 / tct_hhinc12) - 100,
  chg1218_tct_nonhispwh = tct_nonhispwh18 - tct_nonhispwh12,
  chg1218_tct_medrent = tct_medrent18 - tct_medrent12,
  chg1218_tct_medrent_pct = (tct_medrent18 * 100 / tct_medrent12) - 100,
  chg1218_tct_medhome = tct_medhome18 - tct_medhome12,
  chg1218_tct_medhome_pct = (tct_medhome18 * 100 / tct_medhome12) - 100,
  chg1218_tct_renters = tct_renters18 - tct_renters12)

# Individual criterions
data <- ffx %>% mutate(GEOID = GEOID,
                          NAME.y = NAME.y, 
                          geometry = geometry,
                          basehhinc = ifelse(tct_hhinc12 < acs0812cty$cty_hhinc12, 1, 0),
                          basenoba = ifelse(tct_noba12 > acs0812cty$cty_noba12, 1, 0),
                          basenonwhite = ifelse(tct_nonwhite12 > acs0812cty$cty_nonwhite12, 1, 0),
                          baserenters = ifelse(tct_renters12 > acs0812cty$cty_renters12, 1, 0),
                          chgba = ifelse(chg1218_tct_withba > cty$chg1218_cty_withba, 1, 0),
                          chghhinc = ifelse(chg1218_tct_hhinc_pct > cty$chg1218_cty_hhinc_pct, 1, 0),
                          chgnonhispwh = ifelse(chg1218_tct_nonhispwh > cty$chg1218_cty_nonhispwh, 1, 0),
                          chgmedrent = ifelse(chg1218_tct_medrent_pct > cty$chg1218_cty_medrent_pct, 1, 0),
                          chgmedhome = ifelse(chg1218_tct_medhome_pct > cty$chg1218_cty_medhome_pct, 1, 0))

# Group criteria
data <- data %>% mutate(meets_vuln1218 = ifelse(basehhinc + basenoba + basenonwhite + baserenters > 2, 1, 0),
                        meets_soc1218 = ifelse(chgba == 1 | (chghhinc == 1 & chgnonhispwh == 1), 1, 0),
                        meets_inv1218 = ifelse(chgmedrent == 1 | chgmedhome == 1, 1, 0), 
                        gentrified1218 = ifelse(meets_vuln1218 == 1 & meets_soc1218 == 1 & meets_inv1218 == 1, 1, 0),
                        type1218 = case_when(meets_vuln1218 == 0 ~ "Not vulnerable",
                                             meets_vuln1218 == 1 & gentrified1218 == 0 ~ "Vulnerable, did not gentrify",
                                             meets_vuln1218 == 1 & gentrified1218 == 1 ~ "Vulnerable, gentrified"))

#
# Descriptives & model ----------------------------------------------------------------------------------
#

# Get descriptives at this step in 03_desc.R.


#
# Plotting ----------------------------------------------------------------------------------
#

# Check
table(data$meets_inv1218, useNA = "always")
table(data$meets_vuln1218, useNA = "always")
table(data$meets_soc1218, useNA = "always")

# Prep
data$meets_inv1218 <- factor(data$meets_inv1218, labels = c("No" = 0, "Yes" = 1))
data$meets_vuln1218 <- factor(data$meets_vuln1218, labels = c("No" = 0, "Yes" = 1))
data$meets_soc1218 <- factor(data$meets_soc1218, labels = c("No" = 0, "Yes" = 1))

# One time period
ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = data, size = 0.2, aes(fill = meets_vuln1218)) +
  labs(title = "Fairfax County Tracts\nVulnerable to Gentrification, 2010") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1)) +
  scale_fill_viridis(name = "Meets\nCriterion?", guide = "legend", labels = c("No", "Yes"), discrete = T) 

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = data, size = 0.2, aes(fill = meets_soc1218)) +
  labs(title = "Fairfax County Tracts With\nSocioeconomic Change, 2010-16") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1)) +
  scale_fill_viridis(name = "Meets\nCriterion?", guide = "legend", labels = c("No", "Yes"), discrete = T) 

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = data, size = 0.2, aes(fill = meets_inv1218)) +
  labs(title = "Fairfax County Tracts With\nInvestment Change, 2010-16") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1)) +
  scale_fill_viridis(name = "Meets\nCriterion?", guide = "legend", labels = c("No", "Yes"), discrete = T) 

show_col(viridis_pal(option = "")(10))

ggplot(data = data) +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(aes(fill = type1218), size = 0.2) +
  labs(title = "Fairfax County Tract-Level Gentrification\n2008/12 to 2014/18",
       caption = "Data Source: American Community Survey.\nNote: Data not available for tracts show in light gray.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Status", guide = "legend", values = c("#CCCCCC", "#141E3C", "#EB5F0C"), 
                    na.translate = TRUE, na.value = "FFFFFF")

  
