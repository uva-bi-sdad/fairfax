library(readxl)
library(dplyr)
library(opencage)
library(purrr)
library(sf)
library(tigris)
library(ggplot2)
library(ggthemes)


# Note: Teja's opencage key, please swap if using


#
# Geocode ----------------------------------------------------------------------------------------------------
#

# Read in data
childcare <- read_excel("./data/original/childcare/Licensed and unlicensed child care 12-11-2019.xlsx")

# Construct address
childcare$fulladdress <- paste0(childcare$Address, " ", childcare$City, ", VA ", childcare$Zip)

# Geocode
output <- opencage_forward(placename = childcare$fulladdress[1], key = "821a184b48df4b0fbc08e75ccfb29e12", countrycode = "US", language = "en", limit = 1) # test
output <- map(childcare$fulladdress, opencage_forward, key = "821a184b48df4b0fbc08e75ccfb29e12", countrycode = "US", language = "en", no_annotations = TRUE, limit = 1)

# Save
saveRDS(output, file = "./data/working/childcare_rawgeocode.rds")

# Read
data <- readRDS("./data/working/childcare_rawgeocode.rds")


#
# Inspect & transform ----------------------------------------------------------------------------------------------------
#

# How many results per query
df <- NULL
for (i in 1:1366)
{
  x <- data[[i]]["total_results"]
  df <- rbind(df, data.frame(x))
}
mean(df$total_results)
# All rows have been coded.

# Flatten
df <- NULL
for (i in 1:1366)
{
  x <- as.data.frame(data[[i]][["results"]][c("query", "formatted", "components._type", "confidence", "geometry.lat", "geometry.lng")])
  df <- rbind(df, x)
}
remove(x)
remove(i)

# Inspect confidence
table(df$confidence)

# Join geocode with data
ccfacs <- left_join(childcare, df, by = c("fulladdress" = "query"))


#
# Map ----------------------------------------------------------------------------------------------------
#

# Convert to sf
facilities_sf <- st_as_sf(ccfacs, coords = c("geometry.lng", "geometry.lat"))
st_crs(facilities_sf)

# Get state data
ffx <- tracts(state = 51, county = 059)
ffx <- st_as_sf(ffx)

# Set equal crs
st_crs(ffx)
facilities_sf <- st_set_crs(facilities_sf, st_crs(ffx))

# Filter to high confidence
fac_plot <- facilities_sf %>% filter(confidence == 10)

# Plot
ggplot() +
  geom_sf(data = ffx) +
  geom_sf(data = fac_plot, aes(color = `Fac Type`), size = 1, shape = 21, fill = "darkred") +
  theme_map() +
  labs(title = "Child Care Facility Locations")
