library(dplyr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(viridis)
library(tidyr)
library(stringr)
library(hrbrthemes)
library(tidycensus)
library(sf)
library(readr)
library(scales)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# ZIP to tract. NOTE: USING 2019 ZIP TO TRACT CONVERSION! --------------------------------------------------------------------------------------------------
#

tractzip19 <- read_xlsx("./rivanna_data/original/ziptotract/TRACT_ZIP_092019.xlsx") 

tractzip <- tractzip19 %>% filter(zip == 22306 | zip == 22309 |                    # mount vernon 
                                      zip == 20190 | zip == 20191 |                    # reston
                                      zip == 20170 | zip == 20171 |                    # herndon 
                                      zip == 22041 | zip == 22044 |                    # crossroads area 
                                      zip == 22003 | zip == 22042 | zip == 22312) %>%  # annandale 
              mutate(onname = case_when(zip == 22306 | zip == 22309 ~ "Mount Vernon",
                                         zip == 20190 | zip == 20191 ~ "Reston",
                                         zip == 20170 | zip == 20171 ~ "Herndon",
                                         zip == 22041 | zip == 22044 ~ "Crossroads",
                                         zip == 22003 | zip == 22042 | zip == 22312 ~ "Annandale"))


#
# Get ACS data ---------------------------------------------------------------------------------------
#

# For datasets from 2008-12 to 2013-17
vars0913_1317 <- c(
  "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", 
  "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010",
  "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", # less than HS: (002 to 015)/001
  "B03003_001", "B03003_003",                                           # hispanic: 003/001
  "B02001_001", "B02001_003",                                           # black: 003/001
  "B17020_001", "B17020_002",                                           # in poverty: 002/001
  "B11003_001",  "B11003_010", "B11003_016",                            # single parent: (010+016)/001
  "B23025_003", "B23025_005")                                           # in civilian labor force but unemployed: 005/003

# Get
acs1317 <- get_acs(geography = "tract", state = 51, county = 059, variables = vars0913_1317, year = 2017, survey = "acs5", cache_table = TRUE, 
                   output = "wide", geometry = TRUE, keep_geo_vars = TRUE)

# Join
acs15 <- left_join(acs1317, tractzip, by = c("GEOID" = "tract")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0)) %>%
  mutate(isopp = ifelse((NAME.x == 4810 |
                           NAME.x == 4821 |
                           NAME.x == 4514 |
                           NAME.x == 4515.02 |
                           NAME.x == 4528.01 |
                           NAME.x == 4154.01 |
                           NAME.x == 4215 |
                           NAME.x == 4216 |
                           NAME.x == 4218), 1, 0),
         ozname = case_when(NAME.x == 4810 ~ "Tract 4810",
                            NAME.x == 4821 ~ "Tract 4821",
                            NAME.x == 4514 ~ "Tract 4514",
                            NAME.x == 4515.02 ~ "Tract 4515.02",
                            NAME.x == 4528.01 ~ "Tract 4528.01",
                            NAME.x == 4154.01 ~ "Tract 4154.01",
                            NAME.x == 4215 ~ "Tract 4215",
                            NAME.x == 4216 ~ "Tract 4216",
                            NAME.x == 4218 ~ "Tract 4218"))

# Opportunity neighborhoods
ggplot() +
  geom_sf(data = acs15, aes(fill = onname), size = 0.2) +
  theme_map() + 
  labs(title = "Fairfax County Opportunity Neighborhoods", subtitle = "Neighborhoods defined using 2019 ZIP code equivalent tracts.",
       fill = "Opportunity\nNeighborhood") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1)) +
  scale_fill_viridis_d(option = "cividis")
