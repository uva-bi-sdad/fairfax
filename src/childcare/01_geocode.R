library(tidygeocoder)
library(readxl)
library(dplyr)
library(opencage)
library(purrr)
library(sf)
library(tigris)
library(ggplot2)
library(ggthemes)
library(janitor)


# Note: Teja's opencage key, please swap if using


#
# Geocode ----------------------------------------------------------------------------------------------------
#

# Read in data
childcare <- read_excel("./data/original/childcare/Licensed and unlicensed child care 12-11-2019.xlsx")

# Construct address
childcare$fulladdress <- paste0(childcare$Address, " ", childcare$City, ", VA ", childcare$Zip)

# Geocode with Census first
latlongs <- childcare %>% geocode(fulladdress, lat = latitude, long = longitude, method = "census")
# 72 uncoded

# Geocode the rest with opencage and flatten
uncoded <- latlongs %>% filter(is.na(latitude))
latlongs1 <- map(uncoded$fulladdress, opencage_forward, key = "821a184b48df4b0fbc08e75ccfb29e12", countrycode = "US", 
                 language = "en", no_annotations = TRUE, limit = 1)

df <- NULL
for (i in 1:72)
{
  x <- as.data.frame(latlongs1[[i]][["results"]][c("query", "formatted", "components._type", "confidence", "geometry.lat", "geometry.lng")])
  df <- rbind(df, x)
}
remove(x)
remove(i)

# Inspect confidence
table(df$confidence)

# Join with data
ccfacs <- left_join(latlongs, df, by = c("fulladdress" = "query"))

# Clean up lat/long
ccfacs$latitude <- ifelse(is.na(ccfacs$latitude), ccfacs$geometry.lat, ccfacs$latitude)
ccfacs$longitude <- ifelse(is.na(ccfacs$longitude), ccfacs$geometry.lng, ccfacs$longitude)


#
# Geocode - manual ----------------------------------------------------------------------------------------------------
#

# Inspect confidence != 10, manually coded using Google Maps
test3 <- ccfacs %>% filter(confidence == 3)
ccfacs$latitude[ccfacs$fulladdress == "7802 Dogue India Circle, Lorton, VA 22079"] <- 38.695453
ccfacs$longitude[ccfacs$fulladdress == "7802 Dogue India Circle, Lorton, VA 22079"] <- -77.215245

ccfacs$latitude[ccfacs$fulladdress == "2246 Convent Gardens Court, Reston, VA 20191"] <- 38.939827
ccfacs$longitude[ccfacs$fulladdress == "2246 Convent Gardens Court, Reston, VA 20191"] <- -77.367809

test4 <- ccfacs %>% filter(confidence == 4)
ccfacs$latitude[ccfacs$fulladdress == "6414 Landsdowne Center, Alexandria, VA 22315"] <- 38.740973
ccfacs$longitude[ccfacs$fulladdress == "6414 Landsdowne Center, Alexandria, VA 22315"] <- -77.166165

ccfacs$latitude[ccfacs$fulladdress == "6420 Landsdowne Center, Alexandria, VA 22315"] <- 38.740877
ccfacs$longitude[ccfacs$fulladdress == "6420 Landsdowne Center, Alexandria, VA 22315"] <- -77.165982

ccfacs$latitude[ccfacs$fulladdress == "6100 Redwood Square Center, Centreville, VA 20121"] <- 38.831588
ccfacs$longitude[ccfacs$fulladdress == "6100 Redwood Square Center, Centreville, VA 20121"] <- -77.432745

test5 <- ccfacs %>% filter(confidence == 5)
ccfacs$latitude[ccfacs$fulladdress == "1000 Colonial Farm Road, Gate 5 Mc Lean, VA 22101"] <- 38.951900
ccfacs$longitude[ccfacs$fulladdress == "1000 Colonial Farm Road, Gate 5 Mc Lean, VA 22101"] <- -77.146581

ccfacs$latitude[ccfacs$fulladdress == "2400 Dulles Town Blvd., Herndon, VA 20171"] <- 38.958367
ccfacs$longitude[ccfacs$fulladdress == "2400 Dulles Town Blvd., Herndon, VA 20171"] <- -77.422402

ccfacs$latitude[ccfacs$fulladdress == "Spring Hill Recreation Center, 1239 Spring Hill Road Mc Lean, VA 22102"] <- 38.942702
ccfacs$longitude[ccfacs$fulladdress == "Spring Hill Recreation Center, 1239 Spring Hill Road Mc Lean, VA 22102"] <- -77.228405

test6 <- ccfacs %>% filter(confidence == 6)
ccfacs$latitude[ccfacs$fulladdress == "1301 Collingswood Road, Alexandria, VA 22308"] <- 38.733225
ccfacs$longitude[ccfacs$fulladdress == "1301 Collingswood Road, Alexandria, VA 22308"] <- -77.057092

ccfacs$latitude[ccfacs$fulladdress == "6200 Interparcel Road, Alexandria, VA 22315"] <- 38.763710
ccfacs$longitude[ccfacs$fulladdress == "6200 Interparcel Road, Alexandria, VA 22315"] <- -77.152437

ccfacs$latitude[ccfacs$fulladdress == "13967 Endeavour Dr, Herndon, VA 20171"] <- 38.919187
ccfacs$longitude[ccfacs$fulladdress == "13967 Endeavour Dr, Herndon, VA 20171"] <- -77.423578

ccfacs$latitude[ccfacs$fulladdress == "7451 Maderira Place, Annandale, VA 22003"] <- 38.828860
ccfacs$longitude[ccfacs$fulladdress == "7451 Maderira Place, Annandale, VA 22003"] <- -77.202628

test7 <- ccfacs %>% filter(confidence == 7)
ccfacs$latitude[ccfacs$fulladdress == "3903 Suite A-1 Fair Ridge Drive, Fairfax, VA 22033"] <- 38.873637
ccfacs$longitude[ccfacs$fulladdress == "3903 Suite A-1 Fair Ridge Drive, Fairfax, VA 22033"] <- -77.372395

ccfacs$latitude[ccfacs$fulladdress == "14155 F & G Sullyfield Circle, Chantilly, VA 20151"] <- 38.890803
ccfacs$longitude[ccfacs$fulladdress == "14155 F & G Sullyfield Circle, Chantilly, VA 20151"] <- -77.436231

ccfacs$latitude[ccfacs$fulladdress == "4455 Brookfield Corp Drive,Suite201, Chantilly, VA 20151"] <- 38.882211
ccfacs$longitude[ccfacs$fulladdress == "4455 Brookfield Corp Drive,Suite201, Chantilly, VA 20151"] <- -77.441759

ccfacs$latitude[ccfacs$fulladdress == "6720-B Union Mill Road, Clifton, VA 20124"] <- 38.805211
ccfacs$longitude[ccfacs$fulladdress == "6720-B Union Mill Road, Clifton, VA 20124"] <- -77.422002

ccfacs$latitude[ccfacs$fulladdress == "204 & 200 Courthouse Road, Vienna, VA 22180"] <- 38.897694
ccfacs$longitude[ccfacs$fulladdress == "204 & 200 Courthouse Road, Vienna, VA 22180"] <- -77.266214

ccfacs$latitude[ccfacs$fulladdress == "7113 Mint Place, apt# 103 Alexandria, VA 22306"] <- 38.762987
ccfacs$longitude[ccfacs$fulladdress == "7113 Mint Place, apt# 103 Alexandria, VA 22306"] <- -77.090405

test8 <- ccfacs %>% filter(confidence == 8)
ccfacs$latitude[ccfacs$fulladdress == "4350 Blue Spring Drive, Chantilly, VA 20151"] <- 38.904234
ccfacs$longitude[ccfacs$fulladdress == "4350 Blue Spring Drive, Chantilly, VA 20151"] <- -77.478419

ccfacs$latitude[ccfacs$fulladdress == "3810 Meredith Drive, Fairfax Station, VA 22039"] <- 38.855964
ccfacs$longitude[ccfacs$fulladdress == "3810 Meredith Drive, Fairfax Station, VA 22039"] <- -77.317624

ccfacs$latitude[ccfacs$fulladdress == "8334 Mt. Vernon Highway, Alexandria, VA 22309"] <- 38.730703
ccfacs$longitude[ccfacs$fulladdress == "8334 Mt. Vernon Highway, Alexandria, VA 22309"] <- -77.092458

ccfacs$latitude[ccfacs$fulladdress == "6200 Burke Center Parkway, Burke, VA 22015"] <- 38.784196
ccfacs$longitude[ccfacs$fulladdress == "6200 Burke Center Parkway, Burke, VA 22015"] <- -77.281154

ccfacs$latitude[ccfacs$fulladdress == "4451 Brookfield Corporation Dr., #201 Chantilly, VA 20151"] <- 38.882100
ccfacs$longitude[ccfacs$fulladdress == "4451 Brookfield Corporation Dr., #201 Chantilly, VA 20151"] <- -77.441047

ccfacs$latitude[ccfacs$fulladdress == "140818 Rose Lodge Place, Chantilly, VA 20151"] <- 38.878887
ccfacs$longitude[ccfacs$fulladdress == "140818 Rose Lodge Place, Chantilly, VA 20151"] <- -77.432079

ccfacs$latitude[ccfacs$fulladdress == "3605 Robert E. Lee Place #T-3, Alexandria, VA 22306"] <- 38.761604
ccfacs$longitude[ccfacs$fulladdress == "3605 Robert E. Lee Place #T-3, Alexandria, VA 22306"] <- -77.093020

test9 <- ccfacs %>% filter(confidence == 9)
ccfacs$latitude[ccfacs$fulladdress == "2501 Gallows Road, Fairfax, VA 22037"] <- 38.889279
ccfacs$longitude[ccfacs$fulladdress == "2501 Gallows Road, Fairfax, VA 22037"] <- -77.225375

ccfacs$latitude[ccfacs$fulladdress == "860 Dranesville Road, Herndon, VA 20170"] <- 38.985139
ccfacs$longitude[ccfacs$fulladdress == "860 Dranesville Road, Herndon, VA 20170"] <- -77.377347

ccfacs$latitude[ccfacs$fulladdress == "4025 Kings Way, Fairfax, VA 22033"] <- 38.875980
ccfacs$longitude[ccfacs$fulladdress == "4025 Kings Way, Fairfax, VA 22033"] <- -77.387068

ccfacs$latitude[ccfacs$fulladdress == "3411 Lees Corner Road, Chantilly, VA 20151"] <- 38.904653
ccfacs$longitude[ccfacs$fulladdress == "3411 Lees Corner Road, Chantilly, VA 20151"] <- -77.418281

ccfacs$latitude[ccfacs$fulladdress == "840 Dranesville Road, Herndon, VA 20170"] <- 38.983820
ccfacs$longitude[ccfacs$fulladdress == "840 Dranesville Road, Herndon, VA 20170"] <- -77.377158

ccfacs$latitude[ccfacs$fulladdress == "9298 Lewis Chapel Road, Lorton, VA 22079"] <- 38.706863
ccfacs$longitude[ccfacs$fulladdress == "9298 Lewis Chapel Road, Lorton, VA 22079"] <- -77.218500

ccfacs$latitude[ccfacs$fulladdress == "3001 Vaden Drive, Fairfax, VA 22031"] <- 38.873407
ccfacs$longitude[ccfacs$fulladdress == "3001 Vaden Drive, Fairfax, VA 22031"] <- -77.272172

ccfacs$latitude[ccfacs$fulladdress == "7601 Little River Turnpike, Annandale, VA 22003"] <- 38.832134
ccfacs$longitude[ccfacs$fulladdress == "7601 Little River Turnpike, Annandale, VA 22003"] <- -77.20755

ccfacs$latitude[ccfacs$fulladdress == "9970 & 9972 Vale Road, Vienna, VA 22181"] <- 38.903914
ccfacs$longitude[ccfacs$fulladdress == "9970 & 9972 Vale Road, Vienna, VA 22181"] <- -77.292273

ccfacs$latitude[ccfacs$fulladdress == "1603 North Washington Plaza, Reston, VA 20190"] <- 38.969292
ccfacs$longitude[ccfacs$fulladdress == "1603 North Washington Plaza, Reston, VA 20190"] <- -77.340691

ccfacs$latitude[ccfacs$fulladdress == "11508 North Shore Drive, Reston, VA 20190"] <- 38.966188
ccfacs$longitude[ccfacs$fulladdress == "11508 North Shore Drive, Reston, VA 20190"] <- -77.344874      

ccfacs$latitude[ccfacs$fulladdress == "715 Marshall Road SW, Vienna, VA 22180"] <- 38.883502
ccfacs$longitude[ccfacs$fulladdress == "715 Marshall Road SW, Vienna, VA 22180"] <- -77.267097

ccfacs$latitude[ccfacs$fulladdress == "11340 Nancyann Way, Fairfax, VA 22030"] <- 38.849501
ccfacs$longitude[ccfacs$fulladdress == "11340 Nancyann Way, Fairfax, VA 22030"] <- -77.340878

ccfacs$latitude[ccfacs$fulladdress == "7016 StrathmoreStreet, Falls Church, VA 22042"] <- 38.861671
ccfacs$longitude[ccfacs$fulladdress == "7016 StrathmoreStreet, Falls Church, VA 22042"] <- -77.188407

ccfacs$latitude[ccfacs$fulladdress == "5201 Heming Avenue, Springfield, VA 22151"] <- 38.811085
ccfacs$longitude[ccfacs$fulladdress == "5201 Heming Avenue, Springfield, VA 22151"] <- -77.203858

ccfacs$latitude[ccfacs$fulladdress == "5801 Booth Drive, Burke, VA 22015"] <- 38.794130
ccfacs$longitude[ccfacs$fulladdress == "5801 Booth Drive, Burke, VA 22015"] <- -77.246666

ccfacs$latitude[ccfacs$fulladdress == "9130 Arlington Blvd, Fairfax, VA 22031"] <- 38.865826
ccfacs$longitude[ccfacs$fulladdress == "9130 Arlington Blvd, Fairfax, VA 22031"] <- -77.261566  

ccfacs$latitude[ccfacs$fulladdress == "2504 Bronze Store Place, Herndon, VA 20171"] <- 38.952989
ccfacs$longitude[ccfacs$fulladdress == "2504 Bronze Store Place, Herndon, VA 20171"] <- -77.423303

ccfacs$latitude[ccfacs$fulladdress == "7003 Highland Street, Springfield, VA 22150"] <- 38.791663
ccfacs$longitude[ccfacs$fulladdress == "7003 Highland Street, Springfield, VA 22150"] <- -77.187683

ccfacs$latitude[ccfacs$fulladdress == "2249 Castle Rock Square, #11C Reston, VA 20191"] <- 38.933318
ccfacs$longitude[ccfacs$fulladdress == "2249 Castle Rock Square, #11C Reston, VA 20191"] <- -77.347872

ccfacs$latitude[ccfacs$fulladdress == "8239 Russell Road, Alexandria, VA 22309"] <- 38.824325
ccfacs$longitude[ccfacs$fulladdress == "8239 Russell Road, Alexandria, VA 22309"] <- -77.065242

ccfacs$latitude[ccfacs$fulladdress == "5831 Oakview Garden Drivw Unit 1432, Falls Church, VA 22041"] <- 38.845365
ccfacs$longitude[ccfacs$fulladdress == "5831 Oakview Garden Drivw Unit 1432, Falls Church, VA 22041"] <- -77.131181


#
# Clean up ----------------------------------------------------------------------------------------------------
#

# Variable names
ccfacs <- ccfacs %>% clean_names()

# Facility types
ccfacs$fac_type <- recode_factor(ccfacs$fac_type, 
                                 "CDC" = "Licensed child day center",
                                 "CCE" = "Religious exempt child day center",
                                 "CCS" = "Licensed child day center operating <4 months/year", 
                                 "CNS" = "Certified pre-school", 
                                 "FDH" = "Licensed family day home", 
                                 "LOH" = "Local ordinance approved family day home", 
                                 "SAH" = "System approved family day home")


#
# Map ----------------------------------------------------------------------------------------------------
#

# Convert to sf
facilities_sf <- st_as_sf(ccfacs, coords = c("longitude", "latitude"))
st_crs(facilities_sf)

# Get state data
ffx <- tracts(state = 51, county = 059)
ffx <- st_as_sf(ffx)

# Set equal crs
st_crs(ffx)
facilities_sf <- st_set_crs(facilities_sf, st_crs(ffx))

# Plot
ggplot() +
  geom_sf(data = ffx, size = 0.1, fill = "#f7f7f7") +
  geom_sf(data = facilities_sf, aes(fill = fac_type), color = NA, shape = 21) +
  theme_map() +
  labs(title = "Fairfax County Child Care Facility Locations", fill = "Facility type") +
  scale_fill_manual(values = c("#74c476", "#31a354", "#006d2c", "#fd8d3c", "#6baed6", "#3182bd", "#08519c")) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9.5))
