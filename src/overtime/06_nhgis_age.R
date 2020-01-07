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
data <- read_csv("./data/original/nhgis/nhgis_ffx_cty_age.csv")

# Fairfax county only, not city
fairfax <- data %>% filter(COUNTY == "Fairfax County")

# Make variables easier to work with
fairfax <- fairfax %>% transmute(
  # Total persons
  total1970 = AV0AA1970,
  total1980 = AV0AA1980,
  total1990 = AV0AA1990,
  total2000 = AV0AA2000,
  total2010 = AV0AA2010,
  total200812 = AV0AA125,
  # Median age
  medage1980 = AR9AA1980,
  medage1990 = AR9AA1990,
  medage2000 = AR9AA2000,
  medage2010 = AR9AA2010,
  # Proportion 65+
  propold1970 = (B57AP1970 + B57AQ1970 + B57AR1970) / total1970,
  propold1980 = (B57AP1980 + B57AQ1970 + B57AR1980) / total1980,
  propold1990 = (B57AP1990 + B57AQ1970 + B57AR1990) / total1990,
  propold2000 = (B57AP2000 + B57AQ2000 + B57AR2000) / total2000,
  propold2010 = (B57AP2010 + B57AQ2010 + B57AR2010) / total2010,
  # Proportion <18
  propyoung1970 = (B57AA1970 + B57AB1970 + B57AC1970 + B57AD1970) / total1970,
  propyoung1980 = (B57AA1980 + B57AB1980 + B57AC1980 + B57AD1980) / total1980,
  propyoung1990 = (B57AA1990 + B57AB1990 + B57AC1990 + B57AD1990) / total1990,
  propyoung2000 = (B57AA2000 + B57AB2000 + B57AC2000 + B57AD2000) / total2000,
  propyoung2010 = (B57AA2010 + B57AB2010 + B57AC2010 + B57AD2010) / total2010,
)


# Add 2015 data
vars1317 <- c(
  "B01003_001", # total population
  "B01002_002", # median age
  "B01001_001", "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_027", "B01001_028", "B01001_029", "B01001_030", # less than 18
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025", "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049" # older than 65
) 

acs1317 <- get_acs(geography = "county", state = 51, county = 059, variables = vars1317, year = 2017, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = FALSE, keep_geo_vars = FALSE)

acs1317 <- acs1317 %>% transmute(
  total2015 = B01003_001E,
  propyoung2015 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E + B01001_027E + B01001_028E + B01001_029E + B01001_030E) / B01001_001E,
  propold2015 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E + B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E)

# Join
fairfax <- as.data.frame(fairfax)
acs1317 <- as.data.frame(acs1317)
fairfax <- base::cbind(fairfax, acs1317)

# Pivot
fairfax_lng <- fairfax %>% pivot_longer(cols = 1:23, values_to = "prop")
fairfax_lng <- fairfax_lng %>% arrange(name)
fairfax_lng <- fairfax_lng %>% mutate(year = str_extract_all(name, "[0-9]{4}", simplify = TRUE),
                                      name = str_extract_all(name, "[a-zA-Z]+", simplify = TRUE))
fairfax_lng$name <- as.factor(fairfax_lng$name)
fairfax_lng$name <- factor(fairfax_lng$name, levels = c("medage", "propold", "propyoung", "total"),
                           labels = c("Median age", "Proportion age 65+", "Proportion age <18", "Total population"))

fairfax_lng <- fairfax_lng %>% filter(name == "Proportion age 65+" | name == "Proportion age <18")

# Plot
ggplot(fairfax_lng, aes(y = prop, x = year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name) +
  theme_ipsum_tw() +
  labs(title = "Fairfax County Age Distribution, 1970-2015", x = "Year", y = "Proportion",
       caption = "Source: 1970-2010 University of Minnesota National Historical GIS; 2013-2017 American Community Survey.\nData are nominally harmonized. County border changes across time may introduce small error in estimates.")


