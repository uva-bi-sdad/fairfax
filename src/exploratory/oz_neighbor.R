library(dplyr)
library(sf)
library(purrr)
library(readr)


#
# Identify neighbors for every zone: 2013-17-------------------------------------------------------------------------------------------------------
#

# Read in
acs1317 <- st_read("./data/working/acs1317/acs1317_tract_calc.shp")

# Identify zones
zones <- acs1317 %>% filter(opprtnt == 1)

# Identify neighbors for every zone
neighbors <- map(1:9, ~ unlist(st_touches(zones[.x, ], acs1317, sparse = TRUE)))
neighbornames <- map(zones$NAME_x, ~ paste0("neighbor", .x))
names(neighbors) <- neighbornames

# Test plot
plot(st_geometry(acs1317), col = "white")
plot(st_geometry(acs1317[neighbors[[3]], ]), add = TRUE, col = "blue")
plot(st_geometry(zones[3, ]), col = "red", add = TRUE)

# Create neighbor indicators
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

# Write out (csv because of shp name truncation)
write_csv(acs1317, "./data/working/acs1317/acs1317_tract_neigh.csv")

# Clean up
rm(list = ls())


#
# Identify neighbors for every zone: 2012-16-------------------------------------------------------------------------------------------------------
#

# Read in
acs1216 <- st_read("./data/working/acs1216/acs1216_tract_calc.shp")

# Identify zones
zones <- acs1216 %>% filter(opprtnt == 1)

# Identify neighbors for every zone
neighbors <- map(1:9, ~ unlist(st_touches(zones[.x, ], acs1216, sparse = TRUE)))
neighbornames <- map(zones$NAME_x, ~ paste0("neighbor", .x))
names(neighbors) <- neighbornames

# Test plot
plot(st_geometry(acs1216), col = "white")
plot(st_geometry(acs1216[neighbors[[3]], ]), add = TRUE, col = "blue")
plot(st_geometry(zones[3, ]), col = "red", add = TRUE)

# Create neighbor indicators
acs1216 <- acs1216 %>% mutate(id = row_number(),
                              neighbor4215 = ifelse(id %in% neighbors[[3]], 1, 0),
                              neighbor4810 = ifelse(id %in% neighbors[[9]], 1, 0),
                              neighbor4216 = ifelse(id %in% neighbors[[4]], 1, 0),
                              neighbor4515.02 = ifelse(id %in% neighbors[[7]], 1, 0),
                              neighbor4528.01 = ifelse(id %in% neighbors[[8]], 1, 0),
                              neighbor4154.01 = ifelse(id %in% neighbors[[2]], 1, 0),
                              neighbor4218 = ifelse(id %in% neighbors[[5]], 1, 0),
                              neighbor4821 = ifelse(id %in% neighbors[[1]], 1, 0),
                              neighbor4514 = ifelse(id %in% neighbors[[6]], 1, 0))

# Test plot
plot(st_geometry(acs1216), col = "white")
plot(st_geometry(acs1216[acs1216$neighbor4215 == 1, ]), add = TRUE, col = "blue")
plot(st_geometry(acs1216[acs1216$NAME_x == 4215, ]), add = TRUE, col = "red")

# Write out (csv because of shp name truncation)
write_csv(acs1216, "./data/working/acs1216/acs1216_tract_neigh.csv")

# Clean up
rm(list = ls())


#
# Identify neighbors for every zone: 2011-15-------------------------------------------------------------------------------------------------------
#

# Read in
acs1115 <- st_read("./data/working/acs1115/acs1115_tract_calc.shp")

# Identify zones
zones <- acs1115 %>% filter(opprtnt == 1)

# Identify neighbors for every zone
neighbors <- map(1:9, ~ unlist(st_touches(zones[.x, ], acs1115, sparse = TRUE)))
neighbornames <- map(zones$NAME_x, ~ paste0("neighbor", .x))
names(neighbors) <- neighbornames

# Test plot
plot(st_geometry(acs1115), col = "white")
plot(st_geometry(acs1115[neighbors[[3]], ]), add = TRUE, col = "blue")
plot(st_geometry(zones[3, ]), col = "red", add = TRUE)

# Create neighbor indicators
acs1115 <- acs1115 %>% mutate(id = row_number(),
                              neighbor4215 = ifelse(id %in% neighbors[[2]], 1, 0),
                              neighbor4810 = ifelse(id %in% neighbors[[8]], 1, 0),
                              neighbor4216 = ifelse(id %in% neighbors[[3]], 1, 0),
                              neighbor4515.02 = ifelse(id %in% neighbors[[6]], 1, 0),
                              neighbor4528.01 = ifelse(id %in% neighbors[[7]], 1, 0),
                              neighbor4154.01 = ifelse(id %in% neighbors[[1]], 1, 0),
                              neighbor4218 = ifelse(id %in% neighbors[[4]], 1, 0),
                              neighbor4821 = ifelse(id %in% neighbors[[9]], 1, 0),
                              neighbor4514 = ifelse(id %in% neighbors[[5]], 1, 0))

# Test plot
plot(st_geometry(acs1115), col = "white")
plot(st_geometry(acs1115[acs1115$neighbor4215 == 1, ]), add = TRUE, col = "blue")
plot(st_geometry(acs1115[acs1115$NAME_x == 4215, ]), add = TRUE, col = "red")

# Write out (csv because of shp name truncation)
write_csv(acs1115, "./data/working/acs1115/acs1115_tract_neigh.csv")

# Clean up
rm(list = ls())