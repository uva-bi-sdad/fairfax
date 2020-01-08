library(dplyr)
library(readxl)
library(ggplot2)
library(ggthemes)
library(viridis)


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

acs15 <- left_join(acs1317, ziptract15, by = c("GEOID" = "TRACT")) %>% select(GEOID, ZIP, NAME.y, NAME.x,
                                                                              lesshs, hispanic, black, inpoverty, singleparent, 
                                                                              onname, geometry) %>%
                                                                       mutate(ison = ifelse(!is.na(onname), 1, 0)) %>%
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

acs14 <- left_join(acs1216, ziptract14, by = c("GEOID" = "TRACT")) %>% select(GEOID, ZIP, NAME.y, NAME.x,
                                                                              lesshs, hispanic, black, inpoverty, singleparent, 
                                                                              onname, geometry) %>%
                                                                       mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs13 <- left_join(acs1115, ziptract13, by = c("GEOID" = "TRACT")) %>% select(GEOID, ZIP, NAME.y, NAME.x,
                                                                              lesshs, hispanic, black, inpoverty, singleparent, 
                                                                              onname, geometry) %>%
                                                                       mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs12 <- left_join(acs1014, ziptract12, by = c("GEOID" = "TRACT")) %>% select(GEOID, ZIP, NAME.y, NAME.x,
                                                                              lesshs, hispanic, black, inpoverty, singleparent, 
                                                                              onname, geometry) %>%
                                                                       mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs11 <- left_join(acs0913, ziptract11, by = c("GEOID" = "TRACT")) %>% select(GEOID, ZIP, NAME.y, NAME.x,
                                                                              lesshs, hispanic, black, inpoverty, singleparent, 
                                                                              onname, geometry) %>%
                                                                       mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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

acs10 <- left_join(acs0812, ziptract10, by = c("GEOID" = "TRACT")) %>% select(GEOID, ZIP, NAME.y, NAME.x,
                                                                              lesshs, hispanic, black, inpoverty, singleparent, 
                                                                              onname, geometry) %>%
                                                                       mutate(ison = ifelse(!is.na(onname), 1, 0))  %>%
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
