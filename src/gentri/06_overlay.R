library(dplyr)
library(sf)

# Read in
revzones <- read_sf("./rivanna_data/original/revitzones/Commercial_Revitalization_Zones-polygon.shp")
undesig <- read_sf("./rivanna_data/original/revitzones/Undesignated_Low_Income_Community-polygon.shp")

# Check
plot(st_geometry(revzones))
plot(st_geometry(undesig))

# Assemble data from 01_gentrif.R for tract gentrification base plot

# Check projections
st_crs(data)
st_crs(ffxgeo)
st_crs(undesig)
st_crs(revzones)

undesig <- st_transform(undesig, crs = st_crs(data))
revzones <- st_transform(revzones, crs = st_crs(data))

# Plot
ggplot(data = data) +
  geom_sf(data = ffxgeo, size = 0.1, fill = "#F0F0F0") +
  geom_sf(aes(fill = type1218), size = 0.1) +
  geom_sf(data = revzones, aes(color = "A"), size = 0.5, fill = NA, show.legend = "line") +
  geom_sf(data = undesig, size = 0.5, aes(color = "B"), fill = NA, show.legend = "line") +
  labs(title = "Fairfax County Tract-Level Gentrification\n2008/12 to 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11), 
        legend.position = "right") +
  scale_color_manual(values = c("A" = "blue", "B" = "purple"), 
                     labels = c("Revitalization zone", "Undesignated low-income community"),
                     name = "Area") +
  scale_fill_manual(name = "Gentrification Status", 
                    values = c("#FCFDBF", "#FEC98D", "#F1605D"), na.value = "#FFFFFF",
                    guide = guide_legend(override.aes = list(linetype = "blank", shape = NA)))

