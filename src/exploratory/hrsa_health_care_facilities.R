library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(ggmap)
library(leaflet)
library(osmdata)
library(janitor)


#
# Import data ----------------------------------------------------------------------------------------------------
#

# Import
data <- read_xlsx("./data/original/hrsa/fairfax_health_care_facilities.xlsx") 
data <- data %>% clean_names()

head(data)
names(data)

# Convert to sf
data_sf <- st_as_sf(data, coords = c("longitude", "latitude"))


#
# Leaflet ----------------------------------------------------------------------------------------------------
#

# Get Fairfax County bounding box
getbb(place_name = "fairfax county, va")

# Leaflet icons and plot
icons <- awesomeIcons(icon = "user-md", 
                      markerColor = "orange", 
                      iconColor = "blue", 
                      library = "fa")

ffxleaf <- leaflet(data = data_sf) %>% 
               addTiles() %>%
               addAwesomeMarkers(icon = icons) %>%
               setView(lng = -77.299025, lat = 38.853183, zoom = 11)
ffxleaf
  