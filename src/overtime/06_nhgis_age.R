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
  # Proportion 65+
  propold1970 = (B57AP1970 + B57AQ1970 + B57AR1970) / total1970,
  propold1980 = (B57AP1980 + B57AQ1970 + B57AR1980) / total1980,
  propold1990 = (B57AP1990 + B57AQ1970 + B57AR1990) / total1990,
  propold2000 = (B57AP2000 + B57AQ2000 + B57AR2000) / total2000,
  propold2010 = (B57AP2010 + B57AQ2010 + B57AR2010) / total2010,
  # Proportion 0-24
  propyoung1970 = (B57AA1970 + B57AB1970 + B57AC1970 + B57AD1970 + B57AE1970 + B57AF1970 + B57AG1970 + B57AH1970) / total1970,
  propyoung1980 = (B57AA1980 + B57AB1980 + B57AC1980 + B57AD1980 + B57AE1980 + B57AF1980 + B57AG1980 + B57AH1980) / total1980,
  propyoung1990 = (B57AA1990 + B57AB1990 + B57AC1990 + B57AD1990 + B57AE1990 + B57AF1990 + B57AG1990 + B57AH1990) / total1990,
  propyoung2000 = (B57AA2000 + B57AB2000 + B57AC2000 + B57AD2000 + B57AE2000 + B57AF2000 + B57AG2000 + B57AH2000) / total2000,
  propyoung2010 = (B57AA2010 + B57AB2010 + B57AC2010 + B57AD2010 + B57AE2010 + B57AF2010 + B57AG2010 + B57AH2010) / total2010,
  # Proportion working age (25-64)
  propwork1970 = (B57AI1970 + B57AJ1970 + B57AK1970 + B57AL1970 + B57AM1970 + B57AN1970 + B57AO1970) / total1970,
  propwork1980 = (B57AI1980 + B57AJ1980 + B57AK1980 + B57AL1980 + B57AM1980 + B57AN1980 + B57AO1980) / total1980,
  propwork1990 = (B57AI1990 + B57AJ1990 + B57AK1990 + B57AL1990 + B57AM1990 + B57AN1990 + B57AO1990) / total1990,
  propwork2000 = (B57AI2000 + B57AJ2000 + B57AK2000 + B57AL2000 + B57AM2000 + B57AN2000 + B57AO2000) / total2000,
  propwork2010 = (B57AI2010 + B57AJ2010 + B57AK2010 + B57AL2010 + B57AM2010 + B57AN2010 + B57AO2010) / total2010
)


# Add 2015 data
vars1317 <- c(
  "B01003_001", # total population
  "B01001_001", # total for age
  "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_007", "B01001_008", "B01001_009", "B01001_010", # 0-24 male
  "B01001_027", "B01001_028", "B01001_029", "B01001_030", "B01001_031", "B01001_032", "B01001_033", "B01001_034",  # 0-24 female
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025", "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049", # older than 65
  "B01001_011", "B01001_012", "B01001_013", "B01001_014", "B01001_015", "B01001_016", "B01001_017", "B01001_018", "B01001_019",  #25-64 male
  "B01001_035", "B01001_036", "B01001_037", "B01001_038", "B01001_039", "B01001_040", "B01001_041", "B01001_042", "B01001_043" #25-64 female
) 

acs1317 <- get_acs(geography = "county", state = 51, county = 059, variables = vars1317, year = 2017, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = FALSE, keep_geo_vars = FALSE)

acs1317 <- acs1317 %>% transmute(
  total2015 = B01003_001E,
  propyoung2015 = (B01001_003E + B01001_004E + B01001_005E + B01001_006E + B01001_007E + B01001_008E + B01001_009E + B01001_010E +
                   B01001_027E + B01001_028E + B01001_029E + B01001_030E + B01001_031E + B01001_032E + B01001_033E + B01001_034E) / B01001_001E,
  propold2015 = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E + B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E,
  propwork2015 = (B01001_011E + B01001_012E + B01001_013E + B01001_014E + B01001_015E + B01001_016E + B01001_017E + B01001_018E + B01001_019E +
                 B01001_035E + B01001_036E + B01001_037E + B01001_038E + B01001_039E + B01001_040E + B01001_041E + B01001_042E + B01001_043E) / B01001_001E)

# Join
fairfax <- as.data.frame(fairfax)
acs1317 <- as.data.frame(acs1317)
fairfax <- base::cbind(fairfax, acs1317)

# Pivot
fairfax_lng <- fairfax %>% pivot_longer(cols = 1:25, values_to = "prop")
fairfax_lng <- fairfax_lng %>% arrange(name)
fairfax_lng <- fairfax_lng %>% mutate(year = str_extract_all(name, "[0-9]{4}", simplify = TRUE),
                                      name = str_extract_all(name, "[a-zA-Z]+", simplify = TRUE))
fairfax_lng$name <- as.factor(fairfax_lng$name)
fairfax_lng$name <- factor(fairfax_lng$name, levels = c("propyoung", "propwork", "propold", "total"),
                           labels = c("Proportion age <25", "Proportion age 25-64", "Proportion age 65+", "Total population"))

fairfax_lng <- fairfax_lng %>% filter(name != "Total population")

# Plot
ggplot(fairfax_lng, aes(y = prop, x = year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name) +
  theme_ipsum_tw() +
  labs(title = "Fairfax County Age Distribution, 1970-2015", x = "Year", y = "Proportion",
       caption = "Source: 1970-2010 University of Minnesota National Historical GIS; 2013-2017 American Community Survey.\nData are nominally harmonized. County border changes across time may introduce small error in estimates.")

# Plot alternative
ggplot(fairfax_lng[fairfax_lng$name != "Total population", ], aes(fill = fct_reorder(name, desc(name)), y = prop, x = year)) + 
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = round(prop, 2)), size = 3, position = position_stack(vjust = 0.5)) +
  theme_ipsum_tw() +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(option = "cividis", begin = 0.5, end = 0.8) +
  labs(title = "Fairfax County Age Distribution, 1970-2015", x = "Year", y = "Proportion", fill = "Age Group",
       caption = "Source: 1970-2010 University of Minnesota National Historical GIS; 2013-2017 American Community Survey.\nData are nominally harmonized. County border changes across time may introduce small error in estimates.")

