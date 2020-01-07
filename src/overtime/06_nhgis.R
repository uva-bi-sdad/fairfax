library(rgdal)
library(tidycensus)
library(tigris)
library(acs)
library(rgeos)
library(raster)
library(dplyr)
library(sf)
library(readr)
library(tidyr)
library(stringr)
library(ggplot2)
library(hrbrthemes)
library(scales)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


# Read in
data <- read_csv("./data/original/nhgis/nhgis_ffx_cty.csv")

# Fairfax county only, not city
fairfax <- data %>% filter(COUNTY == "Fairfax County")

# Make variables easier to work with
fairfax <- fairfax %>% transmute(
  # Identifiers
  gisjoin = GISJOIN, 
  state = STATE, 
  statefp = STATEFP, 
  statenh = STATENH, 
  county = COUNTY, 
  countyfp = COUNTYFP, 
  countynh = COUNTYNH,
  # Total persons
  total1970 = AV0AA1970,
  total1980 = AV0AA1980,
  total1990 = AV0AA1990,
  total2000 = AV0AA2000,
  total2010 = AV0AA2010,
  total200812 = AV0AA125,
  # Black
  black1970 = B18AB1970,
  black1980 = B18AB1980,
  black1990 = B18AB1990,
  black2000 = B18AB2000,
  black2010 = B18AB2010,
  # Hispanic
  hispanic1970 = A35AA1970,
  hispanic1980 = A35AA1980,
  hispanic1990 = A35AA1990,
  hispanic2000 = A35AA2000,
  hispanic2010 = A35AA2010,
  # Less than HS
  lesshs1970 = B69AA1970,
  lesshs1980 = B69AA1980,
  lesshs1990 = B69AA1990,
  lesshs2000 = B69AA2000,
  lesshs200812 = B69AA125, # less than HS 2010 = 5 year 2008-12
  # Unemployed (in labor force)
  unemp1970 = B84AE1970,
  unemp1980 = B84AE1980,
  unemp1990 = B84AE1990,
  unemp2000 = B84AE2000,
  unemp200812 = B84AE125, # unempl 2010 = 5 year 2008-12
  # Single parent family
  single1970 = AG4AE1970 + AG4AI1970,
  single1980 = AG4AE1980 + AG4AI1980,
  single1990 = AG4AE1990 + AG4AI1990,
  single2000 = AG4AE2000 + AG4AI2000,
  single2010 = AG4AE2010 + AG4AI2010,
  # Families denominator
  singleden1970 = A68AA1970,
  singleden1980 = A68AA1980,
  singleden1990 = A68AA1990,
  singleden2000 = A68AA2000,
  singleden2010 = A68AA2010,
  # In poverty
  inpov1970 = CL6AA1970,
  inpov1980 = CL6AA1980,
  inpov1990 = CL6AA1990,
  inpov2000 = CL6AA2000,
  inpov200812 = CL6AA125,    # in poverty 2010 = 5 year 2008-12
  # In poverty denominator
  inpovden1970 = AX6AA1970,
  inpovden1980 = AX6AA1980,
  inpovden1990 = AX6AA1990,
  inpovden2000 = AX6AA2000,
  inpovden200812 = AX6AA125  # in poverty 2010 = 5 year 2008-12
)

# Calculate proportions
fairfax <- fairfax %>% transmute(
  # Totals
  total1970 = total1970,
  total1980 = total1980,
  total1990 = total1990,
  total2000 = total2000,
  total2010 = total2010,
  # Black
  propblack1970 = black1970 / total1970,
  propblack1980 = black1980 / total1980,
  propblack1990 = black1990 / total1990,
  propblack2000 = black2000 / total2000,
  propblack2010 = black2010 / total2010,
  # Hispanic
  prophisp1970 = hispanic1970 / total1970,
  prophisp1980 = hispanic1980 / total1980,
  prophisp1990 = hispanic1990 / total1990,
  prophisp2000 = hispanic2000 / total2000,
  prophisp2010 = hispanic2010 / total2010,
  # Less HS
  proplesshs1970 = lesshs1970 / total1970,
  proplesshs1980 = lesshs1980 / total1980,
  proplesshs1990 = lesshs1990 / total1990,
  proplesshs2000 = lesshs2000 / total2000,
  proplesshs2010 = lesshs200812 / total200812, # less than HS 2010 = 5 year 2008-12
  # Unemployed
  propunemp1970 = unemp1970 / total1970,
  propunemp1980 = unemp1980 / total1980,
  propunemp1990 = unemp1990 / total1990,
  propunemp2000 = unemp2000 / total2000,
  propunemp2010 = unemp200812 / total200812, # unemp 2010 = 5 year 2008-12
  # Single parent
  propsingle1970 = single1970 / singleden1970, 
  propsingle1980 = single1980 / singleden1980, 
  propsingle1990 = single1990 / singleden1990, 
  propsingle2000 = single2000 / singleden2000, 
  propsingle2010 = single2010 / singleden2010, 
  # In poverty
  propinpov1970 = inpov1970 / inpovden1970,
  propinpov1980 = inpov1980 / inpovden1980,
  propinpov1990 = inpov1990 / inpovden1990,
  propinpov2000 = inpov2000 / inpovden2000,
  propinpov2010 = inpov200812 / inpovden200812 # in poverty 2010 = 5 year 2008-12
)

# Add 2015 data
vars1317 <- c(
  "B01003_001", # total population
  "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", 
  "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010",
  "B15003_011", "B15003_012",                                          # less than HS/up to and including grade 9: (002 to 012)/001
  "B03003_001", "B03003_003",                                           # hispanic: 003/001
  "B02001_001", "B02001_003",                                           # black: 003/001
  "B17020_001", "B17020_002",                                           # in poverty: 002/001
  "B11003_001",  "B11003_010", "B11003_016",                          # single parent: (010+016)/001
  "B23025_001", "B23025_005")                                          # in labor force but unemployed: 005/001

acs1317 <- get_acs(geography = "county", state = 51, county = 059, variables = vars1317, year = 2017, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = FALSE, keep_geo_vars = FALSE)

acs1317 <- acs1317 %>% transmute(
  total2015 = B01003_001E,
  proplesshs2015 = (B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
              B15003_010E + B15003_011E + B15003_012E) / B15003_001E,
  prophisp2015 = B03003_003E / B03003_001E,
  propblack2015 = B02001_003E / B02001_001E,
  propinpov2015 = B17020_002E / B17020_001E,
  propsingle2015 = (B11003_010E + B11003_016E) / B11003_001E,
  propunemp2015 = B23025_005E / B23025_001E)
# acs1317 <- acs1317 %>% st_set_geometry(NULL)

# Join
fairfax <- as.data.frame(fairfax)
acs1317 <- as.data.frame(acs1317)
fairfax <- base::cbind(fairfax, acs1317)

# Pivot
fairfax_lng <- fairfax %>% pivot_longer(cols = 1:42, values_to = "prop")
fairfax_lng <- fairfax_lng %>% arrange(name)
fairfax_lng <- fairfax_lng %>% mutate(year = str_extract_all(name, "[0-9]{4}", simplify = TRUE),
                                      name = str_extract_all(name, "[a-zA-Z]+", simplify = TRUE))
fairfax_lng$name <- as.factor(fairfax_lng$name)
fairfax_lng$name <- factor(fairfax_lng$name, levels = c("propunemp", "proplesshs", "propinpov", "propsingle", "propblack", "prophisp", "total"),
                           labels = c("Proportion unemployed", "Proportion <HS education", "Proportion in poverty", "Proportion single-parent families", "Proportion Black",
                                      "Proportion Hispanic", "Total population"))

# Plot
ggplot(fairfax_lng[fairfax_lng$name != "Total population", ], aes(y = prop, x = year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name) +
  theme_ipsum_tw() +
  labs(title = "Fairfax County Select Population Characteristics, 1970-2015", x = "Year", y = "Proportion",
       caption = "Source: 1970-2010 University of Minnesota National Historical GIS; 2013-2017 American Community Survey.\nData are nominally harmonized. County border changes across time may introduce small error in estimates.")

# Plot population growth
ggplot(fairfax_lng[fairfax_lng$name == "Total population", ], aes(y = prop, x = year, group = name, label = prop)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = comma(prop)), size = 5, vjust = -0.5) +
  theme_ipsum_tw() +
  scale_y_continuous(breaks = seq(0, 1200000, 200000), limits = c(0, 1200000), labels = comma) +
  labs(title = "Fairfax County Total Population, 1970-2015", x = "Year", y = "Total population (count)",
       caption = "Source: 1970-2010 University of Minnesota National Historical GIS; 2013-2017 American Community Survey.\nData are nominally harmonized. County border changes across time may introduce small error in estimates.")



