library(dplyr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(viridis)
library(tidyr)
library(stringr)
library(hrbrthemes)

#
# ZIP to tract --------------------------------------------------------------------------------------------------
#

ziptract15 <- read_xlsx("./data/original/ziptotract/ZIP_TRACT_122015.xlsx") %>% select(ZIP, TRACT)
ziptract14 <- read_xlsx("./data/original/ziptotract/ZIP_TRACT_122014.xlsx") %>% select(ZIP, TRACT)
ziptract13 <- read_xlsx("./data/original/ziptotract/ZIP_TRACT_122013.xlsx") %>% select(ZIP, TRACT)
ziptract12 <- read_xlsx("./data/original/ziptotract/ZIP_TRACT_122012.xlsx") %>% select(ZIP, TRACT)
ziptract11 <- read_xlsx("./data/original/ziptotract/ZIP_TRACT_122011.xlsx") %>% select(ZIP, TRACT)
ziptract10 <- read_xlsx("./data/original/ziptotract/ZIP_TRACT_122010.xlsx") %>% select(ZIP, TRACT)

ziptract15 <- ziptract15 %>% filter(ZIP == 22306 | ZIP == 22309 |                    # mount vernon 
                                    ZIP == 20190 | ZIP == 20191 |                    # reston
                                    ZIP == 20170 | ZIP == 20171 |                    # herndon 
                                    ZIP == 22041 | ZIP == 22044 |                    # crossroads area 
                                    ZIP == 22003 | ZIP == 22042 | ZIP == 22312) %>%  # annandale 
                             mutate (onname = case_when(ZIP == 22306 | ZIP == 22309 ~ "Mount Vernon",
                                                        ZIP == 20190 | ZIP == 20191 ~ "Reston",
                                                        ZIP == 20170 | ZIP == 20171 ~ "Herndon",
                                                        ZIP == 22041 | ZIP == 22044 ~ "Crossroads",
                                                        ZIP == 22003 | ZIP == 22042 | ZIP == 22312 ~ "Annandale"))

ziptract14 <- ziptract14 %>% filter(ZIP == 22306 | ZIP == 22309 |                # mount vernon 
                                    ZIP == 20190 | ZIP == 20191 |                # reston
                                    ZIP == 20170 | ZIP == 20171 |                # herndon 
                                    ZIP == 22041 | ZIP == 22044 |                # crossroads area 
                                    ZIP == 22003 | ZIP == 22042 | ZIP == 22312) %>% # annandale 
                            mutate (onname = case_when(ZIP == 22306 | ZIP == 22309 ~ "Mount Vernon",
                                                       ZIP == 20190 | ZIP == 20191 ~ "Reston",
                                                       ZIP == 20170 | ZIP == 20171 ~ "Herndon",
                                                       ZIP == 22041 | ZIP == 22044 ~ "Crossroads",
                                                       ZIP == 22003 | ZIP == 22042 | ZIP == 22312 ~ "Annandale"))

ziptract13 <- ziptract13 %>% filter(ZIP == 22306 | ZIP == 22309 |                # mount vernon 
                                    ZIP == 20190 | ZIP == 20191 |                # reston
                                    ZIP == 20170 | ZIP == 20171 |                # herndon 
                                    ZIP == 22041 | ZIP == 22044 |                # crossroads area 
                                    ZIP == 22003 | ZIP == 22042 | ZIP == 22312) %>% # annandale 
                            mutate (onname = case_when(ZIP == 22306 | ZIP == 22309 ~ "Mount Vernon",
                                                       ZIP == 20190 | ZIP == 20191 ~ "Reston",
                                                       ZIP == 20170 | ZIP == 20171 ~ "Herndon",
                                                       ZIP == 22041 | ZIP == 22044 ~ "Crossroads",
                                                       ZIP == 22003 | ZIP == 22042 | ZIP == 22312 ~ "Annandale"))

ziptract12 <- ziptract12 %>% filter(ZIP == 22306 | ZIP == 22309 |                # mount vernon 
                                    ZIP == 20190 | ZIP == 20191 |                # reston
                                    ZIP == 20170 | ZIP == 20171 |                # herndon 
                                    ZIP == 22041 | ZIP == 22044 |                # crossroads area 
                                    ZIP == 22003 | ZIP == 22042 | ZIP == 22312) %>% # annandale 
                            mutate (onname = case_when(ZIP == 22306 | ZIP == 22309 ~ "Mount Vernon",
                                                       ZIP == 20190 | ZIP == 20191 ~ "Reston",
                                                       ZIP == 20170 | ZIP == 20171 ~ "Herndon",
                                                       ZIP == 22041 | ZIP == 22044 ~ "Crossroads",
                                                       ZIP == 22003 | ZIP == 22042 | ZIP == 22312 ~ "Annandale"))

ziptract11 <- ziptract11 %>% filter(ZIP == 22306 | ZIP == 22309 |                # mount vernon 
                                    ZIP == 20190 | ZIP == 20191 |                # reston
                                    ZIP == 20170 | ZIP == 20171 |                # herndon 
                                    ZIP == 22041 | ZIP == 22044 |                # crossroads area 
                                    ZIP == 22003 | ZIP == 22042 | ZIP == 22312) %>% # annandale 
                            mutate (onname = case_when(ZIP == 22306 | ZIP == 22309 ~ "Mount Vernon",
                                                       ZIP == 20190 | ZIP == 20191 ~ "Reston",
                                                       ZIP == 20170 | ZIP == 20171 ~ "Herndon",
                                                       ZIP == 22041 | ZIP == 22044 ~ "Crossroads",
                                                       ZIP == 22003 | ZIP == 22042 | ZIP == 22312 ~ "Annandale"))

ziptract10 <- ziptract10 %>% filter(ZIP == 22306 | ZIP == 22309 |                # mount vernon 
                                    ZIP == 20190 | ZIP == 20191 |                # reston
                                    ZIP == 20170 | ZIP == 20171 |                # herndon 
                                    ZIP == 22041 | ZIP == 22044 |                # crossroads area 
                                    ZIP == 22003 | ZIP == 22042 | ZIP == 22312) %>% # annandale 
                            mutate (onname = case_when(ZIP == 22306 | ZIP == 22309 ~ "Mount Vernon",
                                                       ZIP == 20190 | ZIP == 20191 ~ "Reston",
                                                       ZIP == 20170 | ZIP == 20171 ~ "Herndon",
                                                       ZIP == 22041 | ZIP == 22044 ~ "Crossroads",
                                                       ZIP == 22003 | ZIP == 22042 | ZIP == 22312 ~ "Annandale"))

#
# Flag tracts in ACS data --------------------------------------------------------------------------------------------------
#

acs15 <- left_join(acs1317, ziptract15, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0)) %>%
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

acs14 <- left_join(acs1216, ziptract14, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs13 <- left_join(acs1115, ziptract13, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs12 <- left_join(acs1014, ziptract12, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs11 <- left_join(acs0913, ziptract11, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs10 <- left_join(acs0812, ziptract10, by = c("GEOID" = "TRACT")) %>% mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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
  labs(title = "Fairfax County Opportunity Neighborhoods", subtitle = "Neighborhoods defined using 2015 ZIP code equivalent tracts.",
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
  labs(title = "Fairfax County Opportunity Zones and Neighborhoods") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1))  +
  scale_fill_identity(name = "Tract Type", guide = "legend", labels = c("Opportunity Neighborhood", "Opportunity Zone")) 


#
# Characteristics across time --------------------------------------------------------------------------------------------------
#

# Annandale
annandale15 <- acs15 %>% filter(onname == "Annandale")
annandale14 <- acs14 %>% filter(onname == "Annandale")
annandale13 <- acs13 %>% filter(onname == "Annandale")
annandale12 <- acs12 %>% filter(onname == "Annandale")
annandale11 <- acs11 %>% filter(onname == "Annandale")
annandale10 <- acs10 %>% filter(onname == "Annandale")

ggplot(annandale15, aes(fill = hispanic)) +
  geom_sf() +
  theme_map()
ggplot(annandale14, aes(fill = hispanic)) +
  geom_sf() +
  theme_map()
ggplot(annandale13, aes(fill = hispanic)) +
  geom_sf() +
  theme_map()
ggplot(annandale12, aes(fill = hispanic)) +
  geom_sf() +
  theme_map()
ggplot(annandale11, aes(fill = hispanic)) +
  geom_sf() +
  theme_map()
ggplot(annandale10, aes(fill = hispanic)) +
  geom_sf() +
  theme_map()

annandale15 <- annandale15 %>% summarize(ntotal15 = sum(B03003_001E),
                                         nhispanic15 = sum(B03003_003E),
                                         nblack15 = sum(B02001_003E),
                                         ninpov15 = sum(B17020_002E),
                                         nlesshs15 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                         B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle15 = sum(B09005_004E + B09005_005E)) %>%
                               mutate(prophisp15 = nhispanic15 / ntotal15,
                                      propblack15 = nblack15 / ntotal15,
                                      propinpov15 = ninpov15 / ntotal15,
                                      proplesshs15 = nlesshs15 / ntotal15,
                                      propsingle15 = nsingle15 / ntotal15) %>% select(ntotal15, prophisp15, propblack15, propinpov15, proplesshs15, propsingle15)

annandale14 <- annandale14 %>% summarize(ntotal14 = sum(B03003_001E),
                                         nhispanic14 = sum(B03003_003E),
                                         nblack14 = sum(B02001_003E),
                                         ninpov14 = sum(B17020_002E),
                                         nlesshs14 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle14 = sum(B09005_004E + B09005_005E)) %>%
                                mutate(prophisp14 = nhispanic14 / ntotal14,
                                       propblack14 = nblack14 / ntotal14,
                                       propinpov14 = ninpov14 / ntotal14,
                                       proplesshs14 = nlesshs14 / ntotal14,
                                       propsingle14 = nsingle14 / ntotal14) %>% select(ntotal14, prophisp14, propblack14, propinpov14, proplesshs14, propsingle14)

annandale13 <- annandale13 %>% summarize(ntotal13 = sum(B03003_001E),
                                         nhispanic13 = sum(B03003_003E),
                                         nblack13 = sum(B02001_003E),
                                         ninpov13 = sum(B17020_002E),
                                         nlesshs13 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle13 = sum(B09005_004E + B09005_005E)) %>%
  mutate(prophisp13 = nhispanic13 / ntotal13,
         propblack13 = nblack13 / ntotal13,
         propinpov13 = ninpov13 / ntotal13,
         proplesshs13 = nlesshs13 / ntotal13,
         propsingle13 = nsingle13 / ntotal13) %>% select(ntotal13, prophisp13, propblack13, propinpov13, proplesshs13, propsingle13)

annandale12 <- annandale12 %>% summarize(ntotal12 = sum(B03003_001E),
                                         nhispanic12 = sum(B03003_003E),
                                         nblack12 = sum(B02001_003E),
                                         ninpov12 = sum(B17020_002E),
                                         nlesshs12 = sum(B15003_002E + B15003_003E +  B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E +
                                                           B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E),
                                         nsingle12 = sum(B09005_004E + B09005_005E)) %>%
  mutate(prophisp12 = nhispanic12 / ntotal12,
         propblack12 = nblack12 / ntotal12,
         propinpov12 = ninpov12 / ntotal12,
         proplesshs12 = nlesshs12 / ntotal12,
         propsingle12 = nsingle12 / ntotal12) %>% select(ntotal12, prophisp12, propblack12, propinpov12, proplesshs12, propsingle12)

annandale <- cbind(annandale12, annandale13, annandale14, annandale15)
annandale <- annandale %>% st_set_geometry(NULL)

# Pivot
annandale_lng <- annandale %>% pivot_longer(cols = 1:24, values_to = "prop")
annandale_lng <- annandale_lng %>% arrange(name)
annandale_lng <- annandale_lng %>% mutate(year = str_extract_all(name, "[0-9]{2}", simplify = TRUE),
                                      name = str_extract_all(name, "[a-zA-Z]+", simplify = TRUE))
annandale_lng$name <- as.factor(annandale_lng$name)
annandale_lng$name <- factor(annandale_lng$name, levels = c("ntotal", "propblack", "prophisp", "propinpov", "proplesshs", "propsingle"),
                           labels = c("Total population", "Proportion Black", "Proportion Hispanic", "Proportion in poverty", "Proportion <HS education",
                                      "Proportion single parent"))

# Plot
ggplot(annandale_lng[annandale_lng$name != "Total population", ], aes(y = prop, x = year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~name) +
  theme_ipsum_tw() +
  labs(title = "Annandale Select Population Characteristics, 2012-2015", x = "Year", y = "Proportion",
       caption = "Source: American Community Survey.")

