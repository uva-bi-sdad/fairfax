library(dplyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(scales)


#
# Prepare data ------------------------------------------------------------------------------------
#

# Read in
acs1317 <- st_read("./data/working/acs1317/acs1317_tract_calc.shp")

# Identify zone neighbors
zones <- acs1317 %>% filter(opprtnt == 1)

neighbors <- st_touches(zones, acs1317, sparse = TRUE)
neighborlist <- unlist(neighbors)

# Test plot
plot(st_geometry(acs1317), col = "white")
plot(st_geometry(acs1317[neighborlist, ]), add = TRUE, col = "blue")
plot(st_geometry(zones), col = "red", add = TRUE)

# Create neighbor indicator
acs1317 <- acs1317 %>% mutate(id = row_number(),
                              neighbor = ifelse(id %in% neighborlist, 1, 0))
table(acs1317$neighbor)

# Test plot again with all indicators in same dataset
plot(st_geometry(acs1317), col = "white")
plot(st_geometry(acs1317[acs1317$neighbor == 1, ]), add = TRUE, col = "blue")
plot(st_geometry(acs1317[acs1317$opprtnt == 1, ]), add = TRUE, col = "red")

# Convert to factor
acs1317$neighbor <- as.factor(acs1317$neighbor)
acs1317$opprtnt <- as.factor(acs1317$opprtnt)


#
# Pretty plot ------------------------------------------------------------------------------------
#

# Get viridis colors
show_col(viridis_pal()(20))

# Get labels: select tracts, get points (ignore warnings), retrieve coords
labelzones <- acs1317[acs1317$opprtnt == 1, ]
labelneighbors <- acs1317[acs1317$neighbor == 1, ]

point_zones <- st_point_on_surface(labelzones)
point_neighbors <- st_point_on_surface(labelneighbors)

coords_zone <- as.data.frame(st_coordinates(point_zones))
coords_zone$name <- point_zones$NAME_x

coords_neighbor <- as.data.frame(st_coordinates(point_neighbors))
coords_neighbor$name <- point_neighbors$NAME_x

# Plot
ggplot() +
  geom_sf(data = acs1317, size = 0.2) +
  geom_sf(data = acs1317[acs1317$neighbor == 1, ], aes(fill = "#440154")) +
  geom_sf(data = acs1317[acs1317$opprtnt == 1, ], aes(fill = "#FDE725"))  +
  geom_label_repel(data = coords_zone, aes(X, Y, label = name), colour = "black", nudge_x = 3, segment.size = 0.5) + 
  labs(title = "Fairfax County Opportunity Zones and Neighboring Tracts") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = c(0.1, 0.1)) +
  scale_fill_identity(name = "Tract Type", guide = "legend", labels = c("Neighboring Tract", "Opportunity Zone")) 

ggsave("./docs/opzones/plotall.png")
