library(dplyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(scales)
library(compareGroups)


#
# Prepare data -------------------------------------------------------------------------------------------------------
#

# Read in
acs1317 <- st_read("./data/working/acs1317/acs1317_tract_calc.shp")

# Identify zones
zones <- acs1317 %>% filter(opprtnt == 1)


#
# Identify neighbors for every zone ------------------------------------------------------------------------------------
# 

# Neighbors 
neighbors <- map(1:9, ~ unlist(st_touches(zones[.x, ], acs1317, sparse = TRUE)))
neighbornames <- map(zones$NAME_x, ~ paste0("neighbor", .x))
names(neighbors) <- neighbornames

# Test plot
plot(st_geometry(acs1317), col = "white")
plot(st_geometry(acs1317[neighbors[[3]], ]), add = TRUE, col = "blue")
plot(st_geometry(zones[3, ]), col = "red", add = TRUE)


#
# Create neighbor indicators ------------------------------------------------------------------------------------
#

# Indicators
acs1317 <- acs1317 %>% mutate(id = row_number(),
                              neighbor4215 = ifelse(id %in% neighbors[[1]], 1, 0),
                              neighbor4810 = ifelse(id %in% neighbors[[2]], 1, 0),
                              neighbor4216 = ifelse(id %in% neighbors[[3]], 1, 0),
                              neighbor4515.02 = ifelse(id %in% neighbors[[4]], 1, 0),
                              neighbor4528.01 = ifelse(id %in% neighbors[[5]], 1, 0),
                              neighbor4154.01 = ifelse(id %in% neighbors[[6]], 1, 0),
                              neighbor4218 = ifelse(id %in% neighbors[[7]], 1, 0),
                              neighbor4821 = ifelse(id %in% neighbors[[8]], 1, 0),
                              neighbor4514 = ifelse(id %in% neighbors[[9]], 1, 0))

# Test plot
plot(st_geometry(acs1317), col = "white")
plot(st_geometry(acs1317[acs1317$neighbor4215 == 1, ]), add = TRUE, col = "blue")
plot(st_geometry(acs1317[acs1317$NAME_x == 4215, ]), add = TRUE, col = "red")