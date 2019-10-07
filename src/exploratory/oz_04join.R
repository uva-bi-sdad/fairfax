library(dplyr)
library(sf)
library(readr)


#
# Read in data --------------------------------------------------------------------
#

acs1317 <- st_read("./data/working/acs1317/acs1317_tract_neigh.shp")
acs1216 <- st_read("./data/working/acs1216/acs1216_tract_neigh.shp")
acs1115 <- st_read("./data/working/acs1115/acs1115_tract_neigh.shp")


#
# All together -------------------------------------------------------------------
#

acsdata <- rbind(acs1317, acs1216, acs1115)
acsdata <- acsdata %>% group_by(GEOID) %>% 
                       arrange(desc(endyear), .by_group = TRUE) %>% 
                       ungroup()