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

ziptract19 <- read_xlsx("./data/original/ziptotract/ZIP_TRACT_092019.xlsx") %>% transmute(ZIP = zip, TRACT = tract)

ziptract19 <- ziptract19 %>% filter(ZIP == 22306 | ZIP == 22309 |                    # mount vernon 
                                      ZIP == 20190 | ZIP == 20191 |                    # reston
                                      ZIP == 20170 | ZIP == 20171 |                    # herndon 
                                      ZIP == 22041 | ZIP == 22044 |                    # crossroads area 
                                      ZIP == 22003 | ZIP == 22042 | ZIP == 22312) %>%  # annandale 
                            mutate (onname = case_when(ZIP == 22306 | ZIP == 22309 ~ "Mount Vernon",
                                                       ZIP == 20190 | ZIP == 20191 ~ "Reston",
                                                       ZIP == 20170 | ZIP == 20171 ~ "Herndon",
                                                       ZIP == 22041 | ZIP == 22044 ~ "Crossroads",
                                                       ZIP == 22003 | ZIP == 22042 | ZIP == 22312 ~ "Annandale"))

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

# For dataset 2008-12
vars0812 <- c(
  "B15003_001", "B15003_002", "B15003_003", "B15003_004", "B15003_005", 
  "B15003_006", "B15003_007", "B15003_008", "B15003_009", "B15003_010",
  "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", # less than HS: (002 to 015)/001
  "B03003_001", "B03003_003",                                           # hispanic: 003/001
  "B02001_001", "B02001_003",                                           # black: 003/001
  "B17025_001", "B17025_002",                                           # in poverty: 002/001
  "B11003_001",  "B11003_010", "B11003_016",                            # single parent: (010+016)/001
  "B23025_003", "B23025_005")                                           # in civilian labor force but unemployed: 005/003

# Get
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


#
# Flag tracts in ACS data. NOTE: USING 2019 ZIP TO TRACT CONVERSION! --------------------------------------------------------------------------------------------------
#

acs15 <- left_join(acs1317, ziptract19, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0)) %>%
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

acs14 <- left_join(acs1216, ziptract19, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs13 <- left_join(acs1115, ziptract19, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs12 <- left_join(acs1014, ziptract19, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs11 <- left_join(acs0913, ziptract19, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs10 <- left_join(acs0812, ziptract19, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

#
# Plot --------------------------------------------------------------------------------------------------
#

# Test
plot(st_geometry(acs15))
plot(st_geometry(acs15[acs15$ison == 1, ]), add = TRUE, col = "blue")

plot(st_geometry(acs14))
plot(st_geometry(acs14[acs14$ison == 1, ]), add = TRUE, col = "blue")

plot(st_geometry(acs13))
plot(st_geometry(acs13[acs13$ison == 1, ]), add = TRUE, col = "blue")

plot(st_geometry(acs12))
plot(st_geometry(acs12[acs12$ison == 1, ]), add = TRUE, col = "blue")

plot(st_geometry(acs11))
plot(st_geometry(acs11[acs11$ison == 1, ]), add = TRUE, col = "blue")

plot(st_geometry(acs10))
plot(st_geometry(acs10[acs10$ison == 1, ]), add = TRUE, col = "blue")

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

# Opportunity zones
ggplot() +
  geom_sf(data = acs15, aes(fill = ozname), size = 0.2) +
  theme_map() + 
  labs(title = "Fairfax County Opportunity Zones", subtitle = "Zones defined using 2010 Census geography.",
       fill = "Opportunity Zone") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1)) +
  scale_fill_viridis_d(option = "cividis")

# Both
ggplot() +
  geom_sf(data = acs15, size = 0.2) +
  geom_sf(data = acs15[acs15$ison == 1, ], aes(fill = "#440154")) +
  geom_sf(data = acs15[acs15$isopp == 1, ], aes(fill = "#FDE725")) +
  labs(title = "Fairfax County Opportunity Zones and Neighborhoods", subtitle = "Neighborhoods defined using 2019 ZIP code equivalent tracts.\nZones defined using 2010 Census geography.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1))  +
  scale_fill_identity(name = "Tract Type", guide = "legend", labels = c("Opportunity Neighborhood", "Opportunity Zone")) 


#
# Characteristics across time: Annandale --------------------------------------------------------------------------------------------------
#

# Annandale
annandale15 <- acs15 %>% filter(onname == "Annandale") %>% st_set_geometry(NULL)
annandale14 <- acs14 %>% filter(onname == "Annandale") %>% st_set_geometry(NULL)
annandale13 <- acs13 %>% filter(onname == "Annandale") %>% st_set_geometry(NULL)
annandale12 <- acs12 %>% filter(onname == "Annandale") %>% st_set_geometry(NULL)
annandale11 <- acs11 %>% filter(onname == "Annandale") %>% st_set_geometry(NULL)
annandale10 <- acs10 %>% filter(onname == "Annandale") %>% st_set_geometry(NULL)

annandale15 <- annandale15 %>% summarize(ntotal15 = sum(B03003_001E),
                                         ncivlabor15 = sum(B23025_003E),
                                         nfam15 = sum(B11003_001E),
                                         nhispanic15 = sum(B03003_003E),
                                         nblack15 = sum(B02001_003E),
                                         ninpov15 = sum(B17020_002E),
                                         nlesshs15 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                         B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle15 = sum(B11003_010E + B11003_016E),
                                         nunempl15 = sum(B23025_005E)) %>%
                               mutate(prophisp2015 = nhispanic15 / ntotal15,
                                      propblack2015 = nblack15 / ntotal15,
                                      propinpov2015 = ninpov15 / ntotal15,
                                      proplesshs2015 = nlesshs15 / ntotal15,
                                      propsingle2015 = nsingle15 / nfam15,
                                      propunempl2015 = nunempl15 / ncivlabor15,
                                      ntotal2015 = ntotal15) %>% 
                              select(ntotal2015, prophisp2015, propblack2015, propinpov2015, proplesshs2015, propsingle2015, propunempl2015)

annandale14 <- annandale14 %>% summarize(ntotal14 = sum(B03003_001E),
                                         ncivlabor14 = sum(B23025_003E),
                                         nfam14 = sum(B11003_001E),
                                         nhispanic14 = sum(B03003_003E),
                                         nblack14 = sum(B02001_003E),
                                         ninpov14 = sum(B17020_002E),
                                         nlesshs14 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle14 = sum(B11003_010E + B11003_016E),
                                         nunempl14 = sum(B23025_005E)) %>%
  mutate(prophisp2014 = nhispanic14 / ntotal14,
         propblack2014 = nblack14 / ntotal14,
         propinpov2014 = ninpov14 / ntotal14,
         proplesshs2014 = nlesshs14 / ntotal14,
         propsingle2014 = nsingle14 / nfam14,
         propunempl2014 = nunempl14 / ncivlabor14,
         ntotal2014 = ntotal14) %>% 
  select(ntotal2014, prophisp2014, propblack2014, propinpov2014, proplesshs2014, propsingle2014, propunempl2014)

annandale13 <- annandale13 %>% summarize(ntotal13 = sum(B03003_001E),
                                         ncivlabor13 = sum(B23025_003E),
                                         nfam13 = sum(B11003_001E),
                                         nhispanic13 = sum(B03003_003E),
                                         nblack13 = sum(B02001_003E),
                                         ninpov13 = sum(B17020_002E),
                                         nlesshs13 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle13 = sum(B11003_010E + B11003_016E),
                                         nunempl13 = sum(B23025_005E)) %>%
  mutate(prophisp2013 = nhispanic13 / ntotal13,
         propblack2013 = nblack13 / ntotal13,
         propinpov2013 = ninpov13 / ntotal13,
         proplesshs2013 = nlesshs13 / ntotal13,
         propsingle2013 = nsingle13 / nfam13,
         propunempl2013 = nunempl13 / ncivlabor13,
         ntotal2013 = ntotal13) %>% 
  select(ntotal2013, prophisp2013, propblack2013, propinpov2013, proplesshs2013, propsingle2013, propunempl2013)

annandale12 <- annandale12 %>% summarize(ntotal12 = sum(B03003_001E),
                                         ncivlabor12 = sum(B23025_003E),
                                         nfam12 = sum(B11003_001E),
                                         nhispanic12 = sum(B03003_003E),
                                         nblack12 = sum(B02001_003E),
                                         ninpov12 = sum(B17020_002E),
                                         nlesshs12 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle12 = sum(B11003_010E + B11003_016E),
                                         nunempl12 = sum(B23025_005E)) %>%
  mutate(prophisp2012 = nhispanic12 / ntotal12,
         propblack2012 = nblack12 / ntotal12,
         propinpov2012 = ninpov12 / ntotal12,
         proplesshs2012 = nlesshs12 / ntotal12,
         propsingle2012 = nsingle12 / nfam12,
         propunempl2012 = nunempl12 / ncivlabor12,
         ntotal2012 = ntotal12) %>% 
  select(ntotal2012, prophisp2012, propblack2012, propinpov2012, proplesshs2012, propsingle2012, propunempl2012)

annandale11 <- annandale11 %>% summarize(ntotal11 = sum(B03003_001E),
                                         ncivlabor11 = sum(B23025_003E),
                                         nfam11 = sum(B11003_001E),
                                         nhispanic11 = sum(B03003_003E),
                                         nblack11 = sum(B02001_003E),
                                         ninpov11 = sum(B17020_002E),
                                         nlesshs11 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle11 = sum(B11003_010E + B11003_016E),
                                         nunempl11 = sum(B23025_005E)) %>%
  mutate(prophisp2011 = nhispanic11 / ntotal11,
         propblack2011 = nblack11 / ntotal11,
         propinpov2011 = ninpov11 / ntotal11,
         proplesshs2011 = nlesshs11 / ntotal11,
         propsingle2011 = nsingle11 / nfam11,
         propunempl2011 = nunempl11 / ncivlabor11,
         ntotal2011 = ntotal11) %>% 
  select(ntotal2011, prophisp2011, propblack2011, propinpov2011, proplesshs2011, propsingle2011, propunempl2011)

annandale10 <- annandale10 %>% summarize(ntotal10 = sum(B03003_001E),
                                         npovdet10 = sum(B17025_001E),
                                         ncivlabor10 = sum(B23025_003E),
                                         nfam10 = sum(B11003_001E),
                                         nhispanic10 = sum(B03003_003E),
                                         nblack10 = sum(B02001_003E),
                                         ninpov10 = sum(B17025_002E),
                                         nlesshs10 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle10 = sum(B11003_010E + B11003_016E),
                                         nunempl10 = sum(B23025_005E)) %>%
  mutate(prophisp2010 = nhispanic10 / ntotal10,
         propblack2010 = nblack10 / ntotal10,
         propinpov2010 = ninpov10 / npovdet10,
         proplesshs2010 = nlesshs10 / ntotal10,
         propsingle2010 = nsingle10 / nfam10,
         propunempl2010 = nunempl10 / ncivlabor10,
         ntotal2010 = ntotal10) %>% 
  select(ntotal2010, prophisp2010, propblack2010, propinpov2010, proplesshs2010, propsingle2010, propunempl2010)

annandale <- cbind(annandale10, annandale11, annandale12, annandale13, annandale14, annandale15)

# Pivot
annandale_lng <- annandale %>% pivot_longer(cols = 1:42, values_to = "prop")
annandale_lng <- annandale_lng %>% arrange(name)
annandale_lng <- annandale_lng %>% mutate(year = str_extract_all(name, "[0-9]{4}", simplify = TRUE),
                                      name = str_extract_all(name, "[a-zA-Z]+", simplify = TRUE))
annandale_lng$name <- as.factor(annandale_lng$name)
annandale_lng$name <- factor(annandale_lng$name, levels = c("ntotal", "propblack", "prophisp", "propinpov", "proplesshs", "propsingle", "propunempl"),
                           labels = c("Total population", "Proportion Black", "Proportion Hispanic", "Proportion in poverty", "Proportion <HS education",
                                      "Proportion single parent", "Proportion unemployed"))

# Plot
ggplot(annandale_lng[annandale_lng$name != "Total population", ], aes(y = prop, x = year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name) +
  theme_ipsum_tw() +
  labs(title = "Annandale Select Population Characteristics, 2012-2015", x = "Year", y = "Proportion",
       caption = "Source: American Community Survey.")

#
# Characteristics across time: Mount Vernon --------------------------------------------------------------------------------------------------
#

# Mount Vernon
mountvernon15 <- acs15 %>% filter(onname == "Mount Vernon") %>% st_set_geometry(NULL)
mountvernon14 <- acs14 %>% filter(onname == "Mount Vernon") %>% st_set_geometry(NULL)
mountvernon13 <- acs13 %>% filter(onname == "Mount Vernon") %>% st_set_geometry(NULL)
mountvernon12 <- acs12 %>% filter(onname == "Mount Vernon") %>% st_set_geometry(NULL)
mountvernon11 <- acs11 %>% filter(onname == "Mount Vernon") %>% st_set_geometry(NULL)
mountvernon10 <- acs10 %>% filter(onname == "Mount Vernon") %>% st_set_geometry(NULL)

mountvernon15 <- mountvernon15 %>% summarize(ntotal15 = sum(B03003_001E),
                                         ncivlabor15 = sum(B23025_003E),
                                         nfam15 = sum(B11003_001E),
                                         nhispanic15 = sum(B03003_003E),
                                         nblack15 = sum(B02001_003E),
                                         ninpov15 = sum(B17020_002E),
                                         nlesshs15 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle15 = sum(B11003_010E + B11003_016E),
                                         nunempl15 = sum(B23025_005E)) %>%
  mutate(prophisp2015 = nhispanic15 / ntotal15,
         propblack2015 = nblack15 / ntotal15,
         propinpov2015 = ninpov15 / ntotal15,
         proplesshs2015 = nlesshs15 / ntotal15,
         propsingle2015 = nsingle15 / nfam15,
         propunempl2015 = nunempl15 / ncivlabor15,
         ntotal2015 = ntotal15) %>% 
  select(ntotal2015, prophisp2015, propblack2015, propinpov2015, proplesshs2015, propsingle2015, propunempl2015)

mountvernon14 <- mountvernon14 %>% summarize(ntotal14 = sum(B03003_001E),
                                         ncivlabor14 = sum(B23025_003E),
                                         nfam14 = sum(B11003_001E),
                                         nhispanic14 = sum(B03003_003E),
                                         nblack14 = sum(B02001_003E),
                                         ninpov14 = sum(B17020_002E),
                                         nlesshs14 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle14 = sum(B11003_010E + B11003_016E),
                                         nunempl14 = sum(B23025_005E)) %>%
  mutate(prophisp2014 = nhispanic14 / ntotal14,
         propblack2014 = nblack14 / ntotal14,
         propinpov2014 = ninpov14 / ntotal14,
         proplesshs2014 = nlesshs14 / ntotal14,
         propsingle2014 = nsingle14 / nfam14,
         propunempl2014 = nunempl14 / ncivlabor14,
         ntotal2014 = ntotal14) %>% 
  select(ntotal2014, prophisp2014, propblack2014, propinpov2014, proplesshs2014, propsingle2014, propunempl2014)

mountvernon13 <- mountvernon13 %>% summarize(ntotal13 = sum(B03003_001E),
                                         ncivlabor13 = sum(B23025_003E),
                                         nfam13 = sum(B11003_001E),
                                         nhispanic13 = sum(B03003_003E),
                                         nblack13 = sum(B02001_003E),
                                         ninpov13 = sum(B17020_002E),
                                         nlesshs13 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle13 = sum(B11003_010E + B11003_016E),
                                         nunempl13 = sum(B23025_005E)) %>%
  mutate(prophisp2013 = nhispanic13 / ntotal13,
         propblack2013 = nblack13 / ntotal13,
         propinpov2013 = ninpov13 / ntotal13,
         proplesshs2013 = nlesshs13 / ntotal13,
         propsingle2013 = nsingle13 / nfam13,
         propunempl2013 = nunempl13 / ncivlabor13,
         ntotal2013 = ntotal13) %>% 
  select(ntotal2013, prophisp2013, propblack2013, propinpov2013, proplesshs2013, propsingle2013, propunempl2013)

mountvernon12 <- mountvernon12 %>% summarize(ntotal12 = sum(B03003_001E),
                                         ncivlabor12 = sum(B23025_003E),
                                         nfam12 = sum(B11003_001E),
                                         nhispanic12 = sum(B03003_003E),
                                         nblack12 = sum(B02001_003E),
                                         ninpov12 = sum(B17020_002E),
                                         nlesshs12 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle12 = sum(B11003_010E + B11003_016E),
                                         nunempl12 = sum(B23025_005E)) %>%
  mutate(prophisp2012 = nhispanic12 / ntotal12,
         propblack2012 = nblack12 / ntotal12,
         propinpov2012 = ninpov12 / ntotal12,
         proplesshs2012 = nlesshs12 / ntotal12,
         propsingle2012 = nsingle12 / nfam12,
         propunempl2012 = nunempl12 / ncivlabor12,
         ntotal2012 = ntotal12) %>% 
  select(ntotal2012, prophisp2012, propblack2012, propinpov2012, proplesshs2012, propsingle2012, propunempl2012)

mountvernon11 <- mountvernon11 %>% summarize(ntotal11 = sum(B03003_001E),
                                         ncivlabor11 = sum(B23025_003E),
                                         nfam11 = sum(B11003_001E),
                                         nhispanic11 = sum(B03003_003E),
                                         nblack11 = sum(B02001_003E),
                                         ninpov11 = sum(B17020_002E),
                                         nlesshs11 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle11 = sum(B11003_010E + B11003_016E),
                                         nunempl11 = sum(B23025_005E)) %>%
  mutate(prophisp2011 = nhispanic11 / ntotal11,
         propblack2011 = nblack11 / ntotal11,
         propinpov2011 = ninpov11 / ntotal11,
         proplesshs2011 = nlesshs11 / ntotal11,
         propsingle2011 = nsingle11 / nfam11,
         propunempl2011 = nunempl11 / ncivlabor11,
         ntotal2011 = ntotal11) %>% 
  select(ntotal2011, prophisp2011, propblack2011, propinpov2011, proplesshs2011, propsingle2011, propunempl2011)

mountvernon10 <- mountvernon10 %>% summarize(ntotal10 = sum(B03003_001E),
                                         npovdet10 = sum(B17025_001E),
                                         ncivlabor10 = sum(B23025_003E),
                                         nfam10 = sum(B11003_001E),
                                         nhispanic10 = sum(B03003_003E),
                                         nblack10 = sum(B02001_003E),
                                         ninpov10 = sum(B17025_002E),
                                         nlesshs10 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle10 = sum(B11003_010E + B11003_016E),
                                         nunempl10 = sum(B23025_005E)) %>%
  mutate(prophisp2010 = nhispanic10 / ntotal10,
         propblack2010 = nblack10 / ntotal10,
         propinpov2010 = ninpov10 / npovdet10,
         proplesshs2010 = nlesshs10 / ntotal10,
         propsingle2010 = nsingle10 / nfam10,
         propunempl2010 = nunempl10 / ncivlabor10,
         ntotal2010 = ntotal10) %>% 
  select(ntotal2010, prophisp2010, propblack2010, propinpov2010, proplesshs2010, propsingle2010, propunempl2010)

mountvernon <- cbind(mountvernon10, mountvernon11, mountvernon12, mountvernon13, mountvernon14, mountvernon15)

# Pivot
mountvernon_lng <- mountvernon %>% pivot_longer(cols = 1:42, values_to = "prop")
mountvernon_lng <- mountvernon_lng %>% arrange(name)
mountvernon_lng <- mountvernon_lng %>% mutate(year = str_extract_all(name, "[0-9]{4}", simplify = TRUE),
                                          name = str_extract_all(name, "[a-zA-Z]+", simplify = TRUE))
mountvernon_lng$name <- as.factor(mountvernon_lng$name)
mountvernon_lng$name <- factor(mountvernon_lng$name, levels = c("ntotal", "propblack", "prophisp", "propinpov", "proplesshs", "propsingle", "propunempl"),
                             labels = c("Total population", "Proportion Black", "Proportion Hispanic", "Proportion in poverty", "Proportion <HS education",
                                        "Proportion single parent", "Proportion unemployed"))

# Plot
ggplot(mountvernon_lng[mountvernon_lng$name != "Total population", ], aes(y = prop, x = year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name) +
  theme_ipsum_tw() +
  labs(title = "Mount Vernon Select Population Characteristics, 2012-2015", x = "Year", y = "Proportion",
       caption = "Source: American Community Survey.")

#
# Characteristics across time: Crossroads --------------------------------------------------------------------------------------------------
#

# Mount Vernon
crossroads15 <- acs15 %>% filter(onname == "Crossroads") %>% st_set_geometry(NULL)
crossroads14 <- acs14 %>% filter(onname == "Crossroads") %>% st_set_geometry(NULL)
crossroads13 <- acs13 %>% filter(onname == "Crossroads") %>% st_set_geometry(NULL)
crossroads12 <- acs12 %>% filter(onname == "Crossroads") %>% st_set_geometry(NULL)
crossroads11 <- acs11 %>% filter(onname == "Crossroads") %>% st_set_geometry(NULL)
crossroads10 <- acs10 %>% filter(onname == "Crossroads") %>% st_set_geometry(NULL)

crossroads15 <- crossroads15 %>% summarize(ntotal15 = sum(B03003_001E),
                                             ncivlabor15 = sum(B23025_003E),
                                             nfam15 = sum(B11003_001E),
                                             nhispanic15 = sum(B03003_003E),
                                             nblack15 = sum(B02001_003E),
                                             ninpov15 = sum(B17020_002E),
                                             nlesshs15 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                               B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                             nsingle15 = sum(B11003_010E + B11003_016E),
                                             nunempl15 = sum(B23025_005E)) %>%
  mutate(prophisp2015 = nhispanic15 / ntotal15,
         propblack2015 = nblack15 / ntotal15,
         propinpov2015 = ninpov15 / ntotal15,
         proplesshs2015 = nlesshs15 / ntotal15,
         propsingle2015 = nsingle15 / nfam15,
         propunempl2015 = nunempl15 / ncivlabor15,
         ntotal2015 = ntotal15) %>% 
  select(ntotal2015, prophisp2015, propblack2015, propinpov2015, proplesshs2015, propsingle2015, propunempl2015)

crossroads14 <- crossroads14 %>% summarize(ntotal14 = sum(B03003_001E),
                                             ncivlabor14 = sum(B23025_003E),
                                             nfam14 = sum(B11003_001E),
                                             nhispanic14 = sum(B03003_003E),
                                             nblack14 = sum(B02001_003E),
                                             ninpov14 = sum(B17020_002E),
                                             nlesshs14 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                               B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                             nsingle14 = sum(B11003_010E + B11003_016E),
                                             nunempl14 = sum(B23025_005E)) %>%
  mutate(prophisp2014 = nhispanic14 / ntotal14,
         propblack2014 = nblack14 / ntotal14,
         propinpov2014 = ninpov14 / ntotal14,
         proplesshs2014 = nlesshs14 / ntotal14,
         propsingle2014 = nsingle14 / nfam14,
         propunempl2014 = nunempl14 / ncivlabor14,
         ntotal2014 = ntotal14) %>% 
  select(ntotal2014, prophisp2014, propblack2014, propinpov2014, proplesshs2014, propsingle2014, propunempl2014)

crossroads13 <- crossroads13 %>% summarize(ntotal13 = sum(B03003_001E),
                                             ncivlabor13 = sum(B23025_003E),
                                             nfam13 = sum(B11003_001E),
                                             nhispanic13 = sum(B03003_003E),
                                             nblack13 = sum(B02001_003E),
                                             ninpov13 = sum(B17020_002E),
                                             nlesshs13 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                               B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                             nsingle13 = sum(B11003_010E + B11003_016E),
                                             nunempl13 = sum(B23025_005E)) %>%
  mutate(prophisp2013 = nhispanic13 / ntotal13,
         propblack2013 = nblack13 / ntotal13,
         propinpov2013 = ninpov13 / ntotal13,
         proplesshs2013 = nlesshs13 / ntotal13,
         propsingle2013 = nsingle13 / nfam13,
         propunempl2013 = nunempl13 / ncivlabor13,
         ntotal2013 = ntotal13) %>% 
  select(ntotal2013, prophisp2013, propblack2013, propinpov2013, proplesshs2013, propsingle2013, propunempl2013)

crossroads12 <- crossroads12 %>% summarize(ntotal12 = sum(B03003_001E),
                                             ncivlabor12 = sum(B23025_003E),
                                             nfam12 = sum(B11003_001E),
                                             nhispanic12 = sum(B03003_003E),
                                             nblack12 = sum(B02001_003E),
                                             ninpov12 = sum(B17020_002E),
                                             nlesshs12 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                               B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                             nsingle12 = sum(B11003_010E + B11003_016E),
                                             nunempl12 = sum(B23025_005E)) %>%
  mutate(prophisp2012 = nhispanic12 / ntotal12,
         propblack2012 = nblack12 / ntotal12,
         propinpov2012 = ninpov12 / ntotal12,
         proplesshs2012 = nlesshs12 / ntotal12,
         propsingle2012 = nsingle12 / nfam12,
         propunempl2012 = nunempl12 / ncivlabor12,
         ntotal2012 = ntotal12) %>% 
  select(ntotal2012, prophisp2012, propblack2012, propinpov2012, proplesshs2012, propsingle2012, propunempl2012)

crossroads11 <- crossroads11 %>% summarize(ntotal11 = sum(B03003_001E),
                                             ncivlabor11 = sum(B23025_003E),
                                             nfam11 = sum(B11003_001E),
                                             nhispanic11 = sum(B03003_003E),
                                             nblack11 = sum(B02001_003E),
                                             ninpov11 = sum(B17020_002E),
                                             nlesshs11 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                               B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                             nsingle11 = sum(B11003_010E + B11003_016E),
                                             nunempl11 = sum(B23025_005E)) %>%
  mutate(prophisp2011 = nhispanic11 / ntotal11,
         propblack2011 = nblack11 / ntotal11,
         propinpov2011 = ninpov11 / ntotal11,
         proplesshs2011 = nlesshs11 / ntotal11,
         propsingle2011 = nsingle11 / nfam11,
         propunempl2011 = nunempl11 / ncivlabor11,
         ntotal2011 = ntotal11) %>% 
  select(ntotal2011, prophisp2011, propblack2011, propinpov2011, proplesshs2011, propsingle2011, propunempl2011)

crossroads10 <- crossroads10 %>% summarize(ntotal10 = sum(B03003_001E),
                                             npovdet10 = sum(B17025_001E),
                                             ncivlabor10 = sum(B23025_003E),
                                             nfam10 = sum(B11003_001E),
                                             nhispanic10 = sum(B03003_003E),
                                             nblack10 = sum(B02001_003E),
                                             ninpov10 = sum(B17025_002E),
                                             nlesshs10 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                               B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                             nsingle10 = sum(B11003_010E + B11003_016E),
                                             nunempl10 = sum(B23025_005E)) %>%
  mutate(prophisp2010 = nhispanic10 / ntotal10,
         propblack2010 = nblack10 / ntotal10,
         propinpov2010 = ninpov10 / npovdet10,
         proplesshs2010 = nlesshs10 / ntotal10,
         propsingle2010 = nsingle10 / nfam10,
         propunempl2010 = nunempl10 / ncivlabor10,
         ntotal2010 = ntotal10) %>% 
  select(ntotal2010, prophisp2010, propblack2010, propinpov2010, proplesshs2010, propsingle2010, propunempl2010)

crossroads <- cbind(crossroads10, crossroads11, crossroads12, crossroads13, crossroads14, crossroads15)

# Pivot
crossroads_lng <- crossroads %>% pivot_longer(cols = 1:42, values_to = "prop")
crossroads_lng <- crossroads_lng %>% arrange(name)
crossroads_lng <- crossroads_lng %>% mutate(year = str_extract_all(name, "[0-9]{4}", simplify = TRUE),
                                              name = str_extract_all(name, "[a-zA-Z]+", simplify = TRUE))
crossroads_lng$name <- as.factor(crossroads_lng$name)
crossroads_lng$name <- factor(crossroads_lng$name, levels = c("ntotal", "propblack", "prophisp", "propinpov", "proplesshs", "propsingle", "propunempl"),
                               labels = c("Total population", "Proportion Black", "Proportion Hispanic", "Proportion in poverty", "Proportion <HS education",
                                          "Proportion single parent", "Proportion unemployed"))

# Plot
ggplot(crossroads_lng[crossroads_lng$name != "Total population", ], aes(y = prop, x = year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name) +
  theme_ipsum_tw() +
  labs(title = "Crossroads Select Population Characteristics, 2012-2015", x = "Year", y = "Proportion",
       caption = "Source: American Community Survey.")


#
# Characteristics across time: Reston --------------------------------------------------------------------------------------------------
#

# Reston
reston15 <- acs15 %>% filter(onname == "Reston") %>% st_set_geometry(NULL)
reston14 <- acs14 %>% filter(onname == "Reston") %>% st_set_geometry(NULL)
reston13 <- acs13 %>% filter(onname == "Reston") %>% st_set_geometry(NULL)
reston12 <- acs12 %>% filter(onname == "Reston") %>% st_set_geometry(NULL)
reston11 <- acs11 %>% filter(onname == "Reston") %>% st_set_geometry(NULL)
reston10 <- acs10 %>% filter(onname == "Reston") %>% st_set_geometry(NULL)

reston15 <- reston15 %>% summarize(ntotal15 = sum(B03003_001E),
                                           ncivlabor15 = sum(B23025_003E),
                                           nfam15 = sum(B11003_001E),
                                           nhispanic15 = sum(B03003_003E),
                                           nblack15 = sum(B02001_003E),
                                           ninpov15 = sum(B17020_002E),
                                           nlesshs15 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                             B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                           nsingle15 = sum(B11003_010E + B11003_016E),
                                           nunempl15 = sum(B23025_005E)) %>%
  mutate(prophisp2015 = nhispanic15 / ntotal15,
         propblack2015 = nblack15 / ntotal15,
         propinpov2015 = ninpov15 / ntotal15,
         proplesshs2015 = nlesshs15 / ntotal15,
         propsingle2015 = nsingle15 / nfam15,
         propunempl2015 = nunempl15 / ncivlabor15,
         ntotal2015 = ntotal15) %>% 
  select(ntotal2015, prophisp2015, propblack2015, propinpov2015, proplesshs2015, propsingle2015, propunempl2015)

reston14 <- reston14 %>% summarize(ntotal14 = sum(B03003_001E),
                                           ncivlabor14 = sum(B23025_003E),
                                           nfam14 = sum(B11003_001E),
                                           nhispanic14 = sum(B03003_003E),
                                           nblack14 = sum(B02001_003E),
                                           ninpov14 = sum(B17020_002E),
                                           nlesshs14 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                             B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                           nsingle14 = sum(B11003_010E + B11003_016E),
                                           nunempl14 = sum(B23025_005E)) %>%
  mutate(prophisp2014 = nhispanic14 / ntotal14,
         propblack2014 = nblack14 / ntotal14,
         propinpov2014 = ninpov14 / ntotal14,
         proplesshs2014 = nlesshs14 / ntotal14,
         propsingle2014 = nsingle14 / nfam14,
         propunempl2014 = nunempl14 / ncivlabor14,
         ntotal2014 = ntotal14) %>% 
  select(ntotal2014, prophisp2014, propblack2014, propinpov2014, proplesshs2014, propsingle2014, propunempl2014)

reston13 <- reston13 %>% summarize(ntotal13 = sum(B03003_001E),
                                           ncivlabor13 = sum(B23025_003E),
                                           nfam13 = sum(B11003_001E),
                                           nhispanic13 = sum(B03003_003E),
                                           nblack13 = sum(B02001_003E),
                                           ninpov13 = sum(B17020_002E),
                                           nlesshs13 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                             B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                           nsingle13 = sum(B11003_010E + B11003_016E),
                                           nunempl13 = sum(B23025_005E)) %>%
  mutate(prophisp2013 = nhispanic13 / ntotal13,
         propblack2013 = nblack13 / ntotal13,
         propinpov2013 = ninpov13 / ntotal13,
         proplesshs2013 = nlesshs13 / ntotal13,
         propsingle2013 = nsingle13 / nfam13,
         propunempl2013 = nunempl13 / ncivlabor13,
         ntotal2013 = ntotal13) %>% 
  select(ntotal2013, prophisp2013, propblack2013, propinpov2013, proplesshs2013, propsingle2013, propunempl2013)

reston12 <- reston12 %>% summarize(ntotal12 = sum(B03003_001E),
                                           ncivlabor12 = sum(B23025_003E),
                                           nfam12 = sum(B11003_001E),
                                           nhispanic12 = sum(B03003_003E),
                                           nblack12 = sum(B02001_003E),
                                           ninpov12 = sum(B17020_002E),
                                           nlesshs12 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                             B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                           nsingle12 = sum(B11003_010E + B11003_016E),
                                           nunempl12 = sum(B23025_005E)) %>%
  mutate(prophisp2012 = nhispanic12 / ntotal12,
         propblack2012 = nblack12 / ntotal12,
         propinpov2012 = ninpov12 / ntotal12,
         proplesshs2012 = nlesshs12 / ntotal12,
         propsingle2012 = nsingle12 / nfam12,
         propunempl2012 = nunempl12 / ncivlabor12,
         ntotal2012 = ntotal12) %>% 
  select(ntotal2012, prophisp2012, propblack2012, propinpov2012, proplesshs2012, propsingle2012, propunempl2012)

reston11 <- reston11 %>% summarize(ntotal11 = sum(B03003_001E),
                                           ncivlabor11 = sum(B23025_003E),
                                           nfam11 = sum(B11003_001E),
                                           nhispanic11 = sum(B03003_003E),
                                           nblack11 = sum(B02001_003E),
                                           ninpov11 = sum(B17020_002E),
                                           nlesshs11 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                             B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                           nsingle11 = sum(B11003_010E + B11003_016E),
                                           nunempl11 = sum(B23025_005E)) %>%
  mutate(prophisp2011 = nhispanic11 / ntotal11,
         propblack2011 = nblack11 / ntotal11,
         propinpov2011 = ninpov11 / ntotal11,
         proplesshs2011 = nlesshs11 / ntotal11,
         propsingle2011 = nsingle11 / nfam11,
         propunempl2011 = nunempl11 / ncivlabor11,
         ntotal2011 = ntotal11) %>% 
  select(ntotal2011, prophisp2011, propblack2011, propinpov2011, proplesshs2011, propsingle2011, propunempl2011)

reston10 <- reston10 %>% summarize(ntotal10 = sum(B03003_001E),
                                           npovdet10 = sum(B17025_001E),
                                           ncivlabor10 = sum(B23025_003E),
                                           nfam10 = sum(B11003_001E),
                                           nhispanic10 = sum(B03003_003E),
                                           nblack10 = sum(B02001_003E),
                                           ninpov10 = sum(B17025_002E),
                                           nlesshs10 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                             B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                           nsingle10 = sum(B11003_010E + B11003_016E),
                                           nunempl10 = sum(B23025_005E)) %>%
  mutate(prophisp2010 = nhispanic10 / ntotal10,
         propblack2010 = nblack10 / ntotal10,
         propinpov2010 = ninpov10 / npovdet10,
         proplesshs2010 = nlesshs10 / ntotal10,
         propsingle2010 = nsingle10 / nfam10,
         propunempl2010 = nunempl10 / ncivlabor10,
         ntotal2010 = ntotal10) %>% 
  select(ntotal2010, prophisp2010, propblack2010, propinpov2010, proplesshs2010, propsingle2010, propunempl2010)

reston <- cbind(reston10, reston11, reston12, reston13, reston14, reston15)

# Pivot
reston_lng <- reston %>% pivot_longer(cols = 1:42, values_to = "prop")
reston_lng <- reston_lng %>% arrange(name)
reston_lng <- reston_lng %>% mutate(year = str_extract_all(name, "[0-9]{4}", simplify = TRUE),
                                            name = str_extract_all(name, "[a-zA-Z]+", simplify = TRUE))
reston_lng$name <- as.factor(reston_lng$name)
reston_lng$name <- factor(reston_lng$name, levels = c("ntotal", "propblack", "prophisp", "propinpov", "proplesshs", "propsingle", "propunempl"),
                              labels = c("Total population", "Proportion Black", "Proportion Hispanic", "Proportion in poverty", "Proportion <HS education",
                                         "Proportion single parent", "Proportion unemployed"))

# Plot
ggplot(reston_lng[reston_lng$name != "Total population", ], aes(y = prop, x = year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name) +
  theme_ipsum_tw() +
  labs(title = "Reston Select Population Characteristics, 2012-2015", x = "Year", y = "Proportion",
       caption = "Source: American Community Survey.")



#
# Characteristics across time: Herndon --------------------------------------------------------------------------------------------------
#

# Herndon
herndon15 <- acs15 %>% filter(onname == "Herndon") %>% st_set_geometry(NULL)
herndon14 <- acs14 %>% filter(onname == "Herndon") %>% st_set_geometry(NULL)
herndon13 <- acs13 %>% filter(onname == "Herndon") %>% st_set_geometry(NULL)
herndon12 <- acs12 %>% filter(onname == "Herndon") %>% st_set_geometry(NULL)
herndon11 <- acs11 %>% filter(onname == "Herndon") %>% st_set_geometry(NULL)
herndon10 <- acs10 %>% filter(onname == "Herndon") %>% st_set_geometry(NULL)

herndon15 <- herndon15 %>% summarize(ntotal15 = sum(B03003_001E),
                                   ncivlabor15 = sum(B23025_003E),
                                   nfam15 = sum(B11003_001E),
                                   nhispanic15 = sum(B03003_003E),
                                   nblack15 = sum(B02001_003E),
                                   ninpov15 = sum(B17020_002E),
                                   nlesshs15 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle15 = sum(B11003_010E + B11003_016E),
                                   nunempl15 = sum(B23025_005E)) %>%
  mutate(prophisp2015 = nhispanic15 / ntotal15,
         propblack2015 = nblack15 / ntotal15,
         propinpov2015 = ninpov15 / ntotal15,
         proplesshs2015 = nlesshs15 / ntotal15,
         propsingle2015 = nsingle15 / nfam15,
         propunempl2015 = nunempl15 / ncivlabor15,
         ntotal2015 = ntotal15) %>% 
  select(ntotal2015, prophisp2015, propblack2015, propinpov2015, proplesshs2015, propsingle2015, propunempl2015)

herndon14 <- herndon14 %>% summarize(ntotal14 = sum(B03003_001E),
                                   ncivlabor14 = sum(B23025_003E),
                                   nfam14 = sum(B11003_001E),
                                   nhispanic14 = sum(B03003_003E),
                                   nblack14 = sum(B02001_003E),
                                   ninpov14 = sum(B17020_002E),
                                   nlesshs14 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle14 = sum(B11003_010E + B11003_016E),
                                   nunempl14 = sum(B23025_005E)) %>%
  mutate(prophisp2014 = nhispanic14 / ntotal14,
         propblack2014 = nblack14 / ntotal14,
         propinpov2014 = ninpov14 / ntotal14,
         proplesshs2014 = nlesshs14 / ntotal14,
         propsingle2014 = nsingle14 / nfam14,
         propunempl2014 = nunempl14 / ncivlabor14,
         ntotal2014 = ntotal14) %>% 
  select(ntotal2014, prophisp2014, propblack2014, propinpov2014, proplesshs2014, propsingle2014, propunempl2014)

herndon13 <- herndon13 %>% summarize(ntotal13 = sum(B03003_001E),
                                   ncivlabor13 = sum(B23025_003E),
                                   nfam13 = sum(B11003_001E),
                                   nhispanic13 = sum(B03003_003E),
                                   nblack13 = sum(B02001_003E),
                                   ninpov13 = sum(B17020_002E),
                                   nlesshs13 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle13 = sum(B11003_010E + B11003_016E),
                                   nunempl13 = sum(B23025_005E)) %>%
  mutate(prophisp2013 = nhispanic13 / ntotal13,
         propblack2013 = nblack13 / ntotal13,
         propinpov2013 = ninpov13 / ntotal13,
         proplesshs2013 = nlesshs13 / ntotal13,
         propsingle2013 = nsingle13 / nfam13,
         propunempl2013 = nunempl13 / ncivlabor13,
         ntotal2013 = ntotal13) %>% 
  select(ntotal2013, prophisp2013, propblack2013, propinpov2013, proplesshs2013, propsingle2013, propunempl2013)

herndon12 <- herndon12 %>% summarize(ntotal12 = sum(B03003_001E),
                                   ncivlabor12 = sum(B23025_003E),
                                   nfam12 = sum(B11003_001E),
                                   nhispanic12 = sum(B03003_003E),
                                   nblack12 = sum(B02001_003E),
                                   ninpov12 = sum(B17020_002E),
                                   nlesshs12 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle12 = sum(B11003_010E + B11003_016E),
                                   nunempl12 = sum(B23025_005E)) %>%
  mutate(prophisp2012 = nhispanic12 / ntotal12,
         propblack2012 = nblack12 / ntotal12,
         propinpov2012 = ninpov12 / ntotal12,
         proplesshs2012 = nlesshs12 / ntotal12,
         propsingle2012 = nsingle12 / nfam12,
         propunempl2012 = nunempl12 / ncivlabor12,
         ntotal2012 = ntotal12) %>% 
  select(ntotal2012, prophisp2012, propblack2012, propinpov2012, proplesshs2012, propsingle2012, propunempl2012)

herndon11 <- herndon11 %>% summarize(ntotal11 = sum(B03003_001E),
                                   ncivlabor11 = sum(B23025_003E),
                                   nfam11 = sum(B11003_001E),
                                   nhispanic11 = sum(B03003_003E),
                                   nblack11 = sum(B02001_003E),
                                   ninpov11 = sum(B17020_002E),
                                   nlesshs11 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle11 = sum(B11003_010E + B11003_016E),
                                   nunempl11 = sum(B23025_005E)) %>%
  mutate(prophisp2011 = nhispanic11 / ntotal11,
         propblack2011 = nblack11 / ntotal11,
         propinpov2011 = ninpov11 / ntotal11,
         proplesshs2011 = nlesshs11 / ntotal11,
         propsingle2011 = nsingle11 / nfam11,
         propunempl2011 = nunempl11 / ncivlabor11,
         ntotal2011 = ntotal11) %>% 
  select(ntotal2011, prophisp2011, propblack2011, propinpov2011, proplesshs2011, propsingle2011, propunempl2011)

herndon10 <- herndon10 %>% summarize(ntotal10 = sum(B03003_001E),
                                   npovdet10 = sum(B17025_001E),
                                   ncivlabor10 = sum(B23025_003E),
                                   nfam10 = sum(B11003_001E),
                                   nhispanic10 = sum(B03003_003E),
                                   nblack10 = sum(B02001_003E),
                                   ninpov10 = sum(B17025_002E),
                                   nlesshs10 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle10 = sum(B11003_010E + B11003_016E),
                                   nunempl10 = sum(B23025_005E)) %>%
  mutate(prophisp2010 = nhispanic10 / ntotal10,
         propblack2010 = nblack10 / ntotal10,
         propinpov2010 = ninpov10 / npovdet10,
         proplesshs2010 = nlesshs10 / ntotal10,
         propsingle2010 = nsingle10 / nfam10,
         propunempl2010 = nunempl10 / ncivlabor10,
         ntotal2010 = ntotal10) %>% 
  select(ntotal2010, prophisp2010, propblack2010, propinpov2010, proplesshs2010, propsingle2010, propunempl2010)

herndon <- cbind(herndon10, herndon11, herndon12, herndon13, herndon14, herndon15)

# Pivot
herndon_lng <- herndon %>% pivot_longer(cols = 1:42, values_to = "prop")
herndon_lng <- herndon_lng %>% arrange(name)
herndon_lng <- herndon_lng %>% mutate(year = str_extract_all(name, "[0-9]{4}", simplify = TRUE),
                                    name = str_extract_all(name, "[a-zA-Z]+", simplify = TRUE))
herndon_lng$name <- as.factor(herndon_lng$name)
herndon_lng$name <- factor(herndon_lng$name, levels = c("ntotal", "propblack", "prophisp", "propinpov", "proplesshs", "propsingle", "propunempl"),
                          labels = c("Total population", "Proportion Black", "Proportion Hispanic", "Proportion in poverty", "Proportion <HS education",
                                     "Proportion single parent", "Proportion unemployed"))

# Plot
ggplot(herndon_lng[herndon_lng$name != "Total population", ], aes(y = prop, x = year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name) +
  theme_ipsum_tw() +
  labs(title = "Herndon Select Population Characteristics, 2012-2015", x = "Year", y = "Proportion",
       caption = "Source: American Community Survey.")

#
# Characteristics across time: County --------------------------------------------------------------------------------------------------
#

# County
county15 <- acs1317 %>% st_set_geometry(NULL)
county14 <- acs1216 %>% st_set_geometry(NULL)
county13 <- acs1115 %>% st_set_geometry(NULL)
county12 <- acs1014 %>% st_set_geometry(NULL)
county11 <- acs0913 %>% st_set_geometry(NULL)
county10 <- acs0812 %>% st_set_geometry(NULL)

county15 <- county15 %>% summarize(ntotal15 = sum(B03003_001E),
                                   ncivlabor15 = sum(B23025_003E),
                                   nfam15 = sum(B11003_001E),
                                   nhispanic15 = sum(B03003_003E),
                                   nblack15 = sum(B02001_003E),
                                   ninpov15 = sum(B17020_002E),
                                   nlesshs15 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle15 = sum(B11003_010E + B11003_016E),
                                   nunempl15 = sum(B23025_005E)) %>%
  mutate(prophisp2015 = nhispanic15 / ntotal15,
         propblack2015 = nblack15 / ntotal15,
         propinpov2015 = ninpov15 / ntotal15,
         proplesshs2015 = nlesshs15 / ntotal15,
         propsingle2015 = nsingle15 / nfam15,
         propunempl2015 = nunempl15 / ncivlabor15,
         ntotal2015 = ntotal15) %>% 
  select(ntotal2015, prophisp2015, propblack2015, propinpov2015, proplesshs2015, propsingle2015, propunempl2015)

county14 <- county14 %>% summarize(ntotal14 = sum(B03003_001E),
                                   ncivlabor14 = sum(B23025_003E),
                                   nfam14 = sum(B11003_001E),
                                   nhispanic14 = sum(B03003_003E),
                                   nblack14 = sum(B02001_003E),
                                   ninpov14 = sum(B17020_002E),
                                   nlesshs14 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle14 = sum(B11003_010E + B11003_016E),
                                   nunempl14 = sum(B23025_005E)) %>%
  mutate(prophisp2014 = nhispanic14 / ntotal14,
         propblack2014 = nblack14 / ntotal14,
         propinpov2014 = ninpov14 / ntotal14,
         proplesshs2014 = nlesshs14 / ntotal14,
         propsingle2014 = nsingle14 / nfam14,
         propunempl2014 = nunempl14 / ncivlabor14,
         ntotal2014 = ntotal14) %>% 
  select(ntotal2014, prophisp2014, propblack2014, propinpov2014, proplesshs2014, propsingle2014, propunempl2014)

county13 <- county13 %>% summarize(ntotal13 = sum(B03003_001E),
                                   ncivlabor13 = sum(B23025_003E),
                                   nfam13 = sum(B11003_001E),
                                   nhispanic13 = sum(B03003_003E),
                                   nblack13 = sum(B02001_003E),
                                   ninpov13 = sum(B17020_002E),
                                   nlesshs13 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle13 = sum(B11003_010E + B11003_016E),
                                   nunempl13 = sum(B23025_005E)) %>%
  mutate(prophisp2013 = nhispanic13 / ntotal13,
         propblack2013 = nblack13 / ntotal13,
         propinpov2013 = ninpov13 / ntotal13,
         proplesshs2013 = nlesshs13 / ntotal13,
         propsingle2013 = nsingle13 / nfam13,
         propunempl2013 = nunempl13 / ncivlabor13,
         ntotal2013 = ntotal13) %>% 
  select(ntotal2013, prophisp2013, propblack2013, propinpov2013, proplesshs2013, propsingle2013, propunempl2013)

county12 <- county12 %>% summarize(ntotal12 = sum(B03003_001E),
                                   ncivlabor12 = sum(B23025_003E),
                                   nfam12 = sum(B11003_001E),
                                   nhispanic12 = sum(B03003_003E),
                                   nblack12 = sum(B02001_003E),
                                   ninpov12 = sum(B17020_002E),
                                   nlesshs12 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle12 = sum(B11003_010E + B11003_016E),
                                   nunempl12 = sum(B23025_005E)) %>%
  mutate(prophisp2012 = nhispanic12 / ntotal12,
         propblack2012 = nblack12 / ntotal12,
         propinpov2012 = ninpov12 / ntotal12,
         proplesshs2012 = nlesshs12 / ntotal12,
         propsingle2012 = nsingle12 / nfam12,
         propunempl2012 = nunempl12 / ncivlabor12,
         ntotal2012 = ntotal12) %>% 
  select(ntotal2012, prophisp2012, propblack2012, propinpov2012, proplesshs2012, propsingle2012, propunempl2012)

county11 <- county11 %>% summarize(ntotal11 = sum(B03003_001E),
                                   ncivlabor11 = sum(B23025_003E),
                                   nfam11 = sum(B11003_001E),
                                   nhispanic11 = sum(B03003_003E),
                                   nblack11 = sum(B02001_003E),
                                   ninpov11 = sum(B17020_002E),
                                   nlesshs11 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle11 = sum(B11003_010E + B11003_016E),
                                   nunempl11 = sum(B23025_005E)) %>%
  mutate(prophisp2011 = nhispanic11 / ntotal11,
         propblack2011 = nblack11 / ntotal11,
         propinpov2011 = ninpov11 / ntotal11,
         proplesshs2011 = nlesshs11 / ntotal11,
         propsingle2011 = nsingle11 / nfam11,
         propunempl2011 = nunempl11 / ncivlabor11,
         ntotal2011 = ntotal11) %>% 
  select(ntotal2011, prophisp2011, propblack2011, propinpov2011, proplesshs2011, propsingle2011, propunempl2011)

county10 <- county10 %>% summarize(ntotal10 = sum(B03003_001E),
                                   npovdet10 = sum(B17025_001E),
                                   ncivlabor10 = sum(B23025_003E),
                                   nfam10 = sum(B11003_001E),
                                   nhispanic10 = sum(B03003_003E),
                                   nblack10 = sum(B02001_003E),
                                   ninpov10 = sum(B17025_002E),
                                   nlesshs10 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                     B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                   nsingle10 = sum(B11003_010E + B11003_016E),
                                   nunempl10 = sum(B23025_005E)) %>%
  mutate(prophisp2010 = nhispanic10 / ntotal10,
         propblack2010 = nblack10 / ntotal10,
         propinpov2010 = ninpov10 / npovdet10,
         proplesshs2010 = nlesshs10 / ntotal10,
         propsingle2010 = nsingle10 / nfam10,
         propunempl2010 = nunempl10 / ncivlabor10,
         ntotal2010 = ntotal10) %>% 
  select(ntotal2010, prophisp2010, propblack2010, propinpov2010, proplesshs2010, propsingle2010, propunempl2010)

county <- cbind(county10, county11, county12, county13, county14, county15)

# Pivot
county_lng <- county %>% pivot_longer(cols = 1:42, values_to = "prop")
county_lng <- county_lng %>% arrange(name)
county_lng <- county_lng %>% mutate(year = str_extract_all(name, "[0-9]{4}", simplify = TRUE),
                                    name = str_extract_all(name, "[a-zA-Z]+", simplify = TRUE))
county_lng$name <- as.factor(county_lng$name)
county_lng$name <- factor(county_lng$name, levels = c("ntotal", "propblack", "prophisp", "propinpov", "proplesshs", "propsingle", "propunempl"),
                          labels = c("Total population", "Proportion Black", "Proportion Hispanic", "Proportion in poverty", "Proportion <HS education",
                                     "Proportion single parent", "Proportion unemployed"))

# Plot
ggplot(county_lng[county_lng$name != "Total population", ], aes(y = prop, x = year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name) +
  theme_ipsum_tw() +
  labs(title = "Fairfax County Select Population Characteristics, 2012-2015", x = "Year", y = "Proportion",
       caption = "Source: American Community Survey.")

#
# Characteristics across time: ALL --------------------------------------------------------------------------------------------------
#

# Join
reston$id <- "Reston"
herndon$id <- "Herndon"
crossroads$id <- "Crossroads"
mountvernon$id <- "Mount Vernon"
annandale$id <- "Annandale"
county$id <- "Fairfax County"

neighborhoods <- rbind(reston, herndon, crossroads, mountvernon, annandale, county)
neighborhoods$id <- as.factor(neighborhoods$id)
neighborhoods$id <- factor(neighborhoods$id, levels = c("Fairfax County", "Annandale", "Crossroads", "Herndon", "Reston", "Mount Vernon"))

# Pivot
neighborhoods_lng <- neighborhoods %>% pivot_longer(cols = 1:42, values_to = "prop")
neighborhoods_lng <- neighborhoods_lng %>% arrange(id)
neighborhoods_lng <- neighborhoods_lng %>% mutate(year = str_extract_all(name, "[0-9]{4}", simplify = TRUE),
                                      name = str_extract_all(name, "[a-zA-Z]+", simplify = TRUE))
neighborhoods_lng$name <- as.factor(neighborhoods_lng$name)
neighborhoods_lng$name <- factor(neighborhoods_lng$name, levels = c("ntotal", "propblack", "prophisp", "propinpov", "proplesshs", "propsingle", "propunempl"),
                           labels = c("Total population", "Proportion Black", "Proportion Hispanic", "Proportion in poverty", "Proportion <HS education",
                                      "Proportion single parent", "Proportion unemployed"))

# Plot
ggplot(neighborhoods_lng[neighborhoods_lng$name != "Total population", ], aes(y = prop, x = year, group = id, color = id)) +
  geom_line(size = 0.8) +
  theme_pander() +
  facet_wrap(~name, scales = "free_y") +
  scale_colour_manual(values = c("#ff0000", "#a6611a", "#dfc27d", "#a9a9a9", "#80cdc1", "#018571")) + 
  scale_y_continuous(labels = number_format(accuracy = 0.02)) +
  labs(title = "Opportunity Neighborhood Select Population Characteristics, 2010-2015", x = "Year", y = "Proportion",
       caption = "Source: American Community Survey.", color = "Opportunity\nNeighborhood")

# Plot population
ggplot(neighborhoods_lng[neighborhoods_lng$name == "Total population" & neighborhoods_lng$id != "Fairfax County", ], aes(y = prop, x = year, group = id, color = id)) +
  geom_line(size = 0.8) +
  theme_pander() +
  scale_colour_manual(values = c("#a6611a", "#dfc27d", "#a9a9a9", "#80cdc1", "#018571")) + 
  scale_y_continuous(breaks = seq(60000, 170000, 25000), limits = c(60000, 170000), labels = comma) +
  labs(title = "Opportunity Neighborhood Population Growth, 2010-2015", x = "Year", y = "Count",
       caption = "Source: American Community Survey.", color = "Opportunity\nNeighborhood")







