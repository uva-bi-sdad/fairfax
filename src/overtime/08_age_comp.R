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
library(forcats)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key

# Vars
vars1317 <- c(
  "B01001_001", # total for age
  "B01001_003", "B01001_004", "B01001_005", "B01001_006", "B01001_007", "B01001_008", "B01001_009", "B01001_010", # 0-24 male
  "B01001_027", "B01001_028", "B01001_029", "B01001_030", "B01001_031", "B01001_032", "B01001_033", "B01001_034",  # 0-24 female
  "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025", "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049", # older than 65
  "B01001_011", "B01001_012", "B01001_013", "B01001_014", "B01001_015", "B01001_016", "B01001_017", "B01001_018", "B01001_019",  #25-64 male
  "B01001_035", "B01001_036", "B01001_037", "B01001_038", "B01001_039", "B01001_040", "B01001_041", "B01001_042", "B01001_043" #25-64 female
) 


# Fairfax
ffx <- get_acs(geography = "county", state = 51, county = 059, variables = vars1317, year = 2017, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = FALSE, keep_geo_vars = FALSE)

ffx  <- ffx  %>% transmute(
  propyoung_ffx = (B01001_003E + B01001_004E + B01001_005E + B01001_006E + B01001_007E + B01001_008E + B01001_009E + B01001_010E +
                     B01001_027E + B01001_028E + B01001_029E + B01001_030E + B01001_031E + B01001_032E + B01001_033E + B01001_034E) / B01001_001E,
  propold_ffx = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E + B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E,
  propwork_ffx = (B01001_011E + B01001_012E + B01001_013E + B01001_014E + B01001_015E + B01001_016E + B01001_017E + B01001_018E + B01001_019E +
                    B01001_035E + B01001_036E + B01001_037E + B01001_038E + B01001_039E + B01001_040E + B01001_041E + B01001_042E + B01001_043E) / B01001_001E)

# Virginia
va <- get_acs(geography = "state", state = 51, variables = vars1317, year = 2017, survey = "acs5", cache_table = TRUE, 
               output = "wide", geometry = FALSE, keep_geo_vars = FALSE)

va  <- va  %>% transmute(
  propyoung_va = (B01001_003E + B01001_004E + B01001_005E + B01001_006E + B01001_007E + B01001_008E + B01001_009E + B01001_010E +
                     B01001_027E + B01001_028E + B01001_029E + B01001_030E + B01001_031E + B01001_032E + B01001_033E + B01001_034E) / B01001_001E,
  propold_va = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E + B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E,
  propwork_va = (B01001_011E + B01001_012E + B01001_013E + B01001_014E + B01001_015E + B01001_016E + B01001_017E + B01001_018E + B01001_019E +
                    B01001_035E + B01001_036E + B01001_037E + B01001_038E + B01001_039E + B01001_040E + B01001_041E + B01001_042E + B01001_043E) / B01001_001E)

# US
usa <- get_acs(geography = "us", variables = vars1317, year = 2017, survey = "acs5", cache_table = TRUE, 
               output = "wide", geometry = FALSE, keep_geo_vars = FALSE)

usa  <- usa  %>% transmute(
  propyoung_usa = (B01001_003E + B01001_004E + B01001_005E + B01001_006E + B01001_007E + B01001_008E + B01001_009E + B01001_010E +
                     B01001_027E + B01001_028E + B01001_029E + B01001_030E + B01001_031E + B01001_032E + B01001_033E + B01001_034E) / B01001_001E,
  propold_usa = (B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E + B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E) / B01001_001E,
  propwork_usa = (B01001_011E + B01001_012E + B01001_013E + B01001_014E + B01001_015E + B01001_016E + B01001_017E + B01001_018E + B01001_019E +
                    B01001_035E + B01001_036E + B01001_037E + B01001_038E + B01001_039E + B01001_040E + B01001_041E + B01001_042E + B01001_043E) / B01001_001E)

# Join
ffx <- as.data.frame(ffx)
va <- as.data.frame(va)
usa <- as.data.frame(usa)
data <- base::cbind(ffx, va, usa)

# Pivot
data_lng <- data %>% pivot_longer(cols = 1:9, values_to = "prop")
data_lng <- data_lng %>% arrange(name)
data_lng <- data_lng %>% mutate(place = str_extract_all(name, "_[^_]+$", simplify = TRUE),
                                      name = str_extract_all(name, "^[^_]+", simplify = TRUE))
data_lng$name <- as.factor(data_lng$name)
data_lng$name <- factor(data_lng$name, levels = c("propyoung", "propwork", "propold"),
                           labels = c("Proportion age <25", "Proportion age 25-64", "Proportion age 65+"))
data_lng$place <- as.factor(data_lng$place)
data_lng$place <- factor(data_lng$place, levels = c("_ffx", "_va", "_usa"),
                        labels = c("Fairfax County", "Virginia", "United States"))

# Plot alternative
ggplot(data_lng, aes(fill = fct_reorder(name, desc(name)), y = prop, x = place)) + 
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = round(prop, 2)), size = 3, position = position_stack(vjust = 0.5)) +
  theme_ipsum_tw() +
  theme(legend.position = "bottom") +
  scale_fill_viridis_d(option = "cividis", begin = 0.5, end = 0.8) +
  labs(title = "Area Age Distribution Comparison", x = "Area", y = "Proportion", fill = "Age Group",
       caption = "Source: 2013-2017 American Community Survey.")


