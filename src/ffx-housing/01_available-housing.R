rm(list = ls())
library(tidygeocoder)
library(tidyverse)
library(dplyr)
library(opencage)
library(purrr)
library(sf)
library(tigris)
library(ggplot2)
library(ggthemes)
library(janitor)

# Read in data
housing_data <- read_csv("/sfs/qumulo/qhome/kb7hp/fairfax/rivanna_data/working/fairfax_publichousing_2020.csv")

#
# Geocode ----------------------------------------------------------------------------------------------------
#

# Geocode with Census first
latlongs <- housing_data %>% geocode(address, lat = latitude, long = longitude, method = "census")

# Geocode the rest with opencage and flatten
uncoded <- latlongs %>% filter(is.na(latitude))
latlongs1 <- map(uncoded$address, opencage_forward, key = "821a184b48df4b0fbc08e75ccfb29e12", countrycode = "US", 
                 language = "en", no_annotations = TRUE, limit = 1)

df <- NULL
for (i in 1:95){
  x <- as.data.frame(latlongs1[[i]][["results"]][c("query", "formatted", "components._type", "confidence", "geometry.lat", "geometry.lng")])
  df <- rbind(df, x)
}
remove(x)
remove(i)

# Inspect confidence
table(df$confidence)

# Join with data
ccfacs <- left_join(latlongs, df, by = c("address" = "query"))


# Manually fix lat and long for select locations 

# The Avant at Reston Town Center
ccfacs$latitude[ccfacs$address == "12025 Town Square Street Reston, VA 20190"] <- 38.958000
ccfacs$longitude[ccfacs$address == "12025 Town Square Street Reston, VA 20190"] <- -77.361110

# Station on Silver
ccfacs$latitude[ccfacs$address == "2340 Carta Way Herndon, VA 20171"] <- 38.958210
ccfacs$longitude[ccfacs$address == "2340 Carta Way Herndon, VA 20171"] <- -77.419200

# The Adaire
ccfacs$latitude[ccfacs$address == "1521 Boyd Pointe Way McLean, VA 22102"] <- 38.930620
ccfacs$longitude[ccfacs$address == "1521 Boyd Pointe Way McLean, VA 22102"] <- -77.240860



# Clean up lat/long
ccfacs$latitude <- ifelse(is.na(ccfacs$latitude), ccfacs$geometry.lat, ccfacs$latitude)
ccfacs$longitude <- ifelse(is.na(ccfacs$longitude), ccfacs$geometry.lng, ccfacs$longitude)
rm(df, latlongs, latlongs1, uncoded)

ccfacs %>% 
  mutate(studios = recode(studios, median(studios) = NA),
         one_br_apts = recode(studios, median(studios) = NA),
         two_br_apts = recode(two_br_apts, median(two_br_apts) = NA),
         three_br_apts = recode(studios, median(three_br_apts) = NA),
         Available_Units = studios + one_br_apts + two_br_apts + three_br_apts)

recode(ccfacs$studios, is.na(studios) = median(studios))

test <- ccfacs %>% 
  mutate(studios = replace_na(studios, 0),
         one_br_apts = replace_na(one_br_apts, 16), 
         two_br_apts = replace_na(two_br_apts, 8), 
         three_br_apts = replace_na(three_br_apts, 2.5),
         Available_Units = studios + one_br_apts + two_br_apts + three_br_apts)
  


#
# Clean up ----------------------------------------------------------------------------------------------------
#

ccfacs <- ccfacs %>% 
  mutate(Policy = recode(policy_initiative,
                         "Affordable Dwelling Unit Rental Program" = "Affordable Dwelling",
                         "Magnet Housing" = "Magnet Housing",
                         "RAD-PBV/HCV/FCRP" = "Public/Voucher Housing",
                         "Senior Housing" = "Senior Housing",
                         "Workforce Dwelling Unit Rental Program" = "Workforce Dwelling")) %>% 
  mutate(policy_initiative = as.factor(policy_initiative))

ccfacs$Policy

#
# Map ----------------------------------------------------------------------------------------------------
#


facilities_sf <- st_as_sf(ccfacs, coords = c("longitude", "latitude"))
st_crs(facilities_sf)

# Get state data
ffx <- tracts(state = 51, county = 059)
ffx <- st_as_sf(ffx)

# Get state data
ffx <- tracts(state = 51, county = 059)
ffx <- st_as_sf(ffx)

# Set equal crs
st_crs(ffx)
facilities_sf <- st_set_crs(facilities_sf, st_crs(ffx))

ggplot() +
  geom_sf(data = ffx, size = 0.1, fill = "#f7f7f7") +
  geom_sf(data = facilities_sf, aes(fill = Policy), shape = 21, size = 3, show.legend = "point") +
  #geom_sf_label(data = facilities_sf, aes(label = location)) +
  theme_map() +
  labs(title = "Public Housing in Fairfax County, VA", fill = "Policy") +
  scale_fill_manual(values = c("#006dd8", "#ff8c00", "#ffcb0c", "#8b0000", "#00b300")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 9.5))



ggplot() +
  geom_sf(data = ffx, size = 0.1, fill = "#f7f7f7") +
  geom_sf(data = facilities_sf, aes(fill = Policy), 
          shape = 21, size = ccfacs$Available_Units/6, alpha = 0.4, show.legend = "point") +
  #geom_sf_label(data = facilities_sf, aes(label = location)) +
  theme_map() +
  labs(title = "Public Housing in Fairfax County, VA", fill = "Policy") +
  scale_fill_manual(values = c("#006dd8", "#ff8c00", "#ffcb0c", "#8b0000", "#00b300")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.text = element_text(size = 9.5))
















