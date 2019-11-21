library(dplyr)
library(leaflet)

# Note: 01-read to read in data, 02-wac_create to create the dataset


#
# Plot ------------------------------------------------------------------------------------
#

# Get Fairfax County bounding box
osmdata::getbb("fairfax county, va")

# Palette
pal <- colorNumeric(palette = "viridis", domain = c(0, 1))

# Leaflet  
leaflet(options = leafletOptions(minZoom = 11)) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -77.2888,
          lat = 38.83079, 
          zoom = 11) %>%
  setMaxBounds(lng1 = -77.53704,
               lat1 = 38.60395,
               lng2 = -77.04057, 
               lat2 = 39.05763) %>%
  addPolygons(data = wac0917geo, 
              smoothFactor = 0,
              fillOpacity = 0.9,
              weight = 0.1,
              color = ~pal(job09_female), 
              group = "Female 2009") %>%
  addPolygons(data = wac0917geo, 
              smoothFactor = 0,
              fillOpacity = 0.9,
              weight = 0.1,
              color = ~pal(job17_female), 
              group = "Female 2017") %>%
  addLayersControl(
    overlayGroups = c("Female 2009", "Female 2017"),
    options = layersControlOptions(collapsed = FALSE)
  )