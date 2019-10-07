library(dplyr)
library(purrr)
library(sf)
library(arsenal)
library(knitr)


#
# Read in & prepare --------------------------------------------------------------------
#

# Read in
acs1317 <- st_read("./data/working/acs1317/acs1317_tract_neigh.shp")

# Create neighbor, neighbor only (not also a zone), and zone indicators
data <- acs1317 %>% mutate(neighbor = ifelse((n4215 == 1 | n4810 == 1 | n4216 == 1 | n4515_02 == 1 | n4528_01 == 1 | n4154_01 == 1 |
                                                n4218 == 1 | n4821 == 1 | n4514 == 1), 1, 0),
                           neighboronly = ifelse((opprtnt == 0 & neighbor == 1), 1, 0),
                           type = case_when(opprtnt == 1 ~ "Zone", neighboronly == 1 ~ "Neighbor"),
                           type = as.factor(type))

# Select zones and neighbors, select variables
data <- data %>% filter(opprtnt == 1 | neighbor == 1)

# Filter to the zone and its neighbors *that are not also a zone*
t4215 <- data %>% filter(NAME_x == 4215 | (n4215 == 1 & opprtnt == 0))
t4810 <- data %>% filter(NAME_x == 4810 | (n4810 == 1 & opprtnt == 0))
t4216 <- data %>% filter(NAME_x == 4216 | (n4216 == 1 & opprtnt == 0))
t4515_02 <- data %>% filter(NAME_x == 4515.02 | (n4515_02 == 1 & opprtnt == 0)) 
t4528_01 <- data %>% filter(NAME_x == 4528.01 | (n4528_01 == 1 & opprtnt == 0))
t4154_01 <- data %>% filter(NAME_x == 4154.01 | (n4154_01 == 1 & opprtnt == 0))
t4218 <- data %>% filter(NAME_x == 4218 | (n4218 == 1 & opprtnt == 0))
t4821 <- data %>% filter(NAME_x == 4821 | (n4821 == 1 & opprtnt == 0))
t4514 <- data %>% filter(NAME_x == 4514 | (n4514 == 1 & opprtnt == 0))

# Test plot
plot(st_geometry(t4810))
plot(st_geometry(t4810[t4810$opprtnt == 1, ]), add = TRUE, col = "red")
plot(st_geometry(t4810[t4810$neighboronly == 1, ]), add = TRUE, col = "blue")

# Drop geography and select variables
thezones <- list("Zone 4215" = t4215, "Zone 4810" = t4810, "Zone 4216" = t4216, "Zone 4515.02" = t4515_02, "Zone 4528.01" = t4528_01, 
                 "Zone 4154.01" = t4154_01, "Zone 4218" = t4218, "Zone 4821" = t4821, "Zone 4514" = t4514)
thezones <- thezones %>% map(~ st_drop_geometry(.x)) %>%
                         map(~ select(.x, GEOID, NAME_x, NAME_y, popultn, hs_r_ls, poverty, ag_65_l, hispanc, black, family, foreign, 
                                      wrkfrmh, lngcmmt, assstnc, labrfrc, vacant, renters, yearblt, rntbrdn, opprtnt, endyear, 
                                      neighbor, neighboronly, type))


#
# Compare zone with neighbors --------------------------------------------------------------------
#

mylabels <- list(popultn = "Population", hs_r_ls = "High school or less (prop.)", poverty = "In poverty (prop.)",
                 ag_65_l = "65 or older (prop.)", hispanc = "Hispanic (prop.)", black = "Black (prop.)", family = "Family households (prop.)", 
                 foreign = "Foreign-born (prop.)", wrkfrmh = "Work from home (prop.)", lngcmmt = "Long commute (prop.)", 
                 assstnc = "Receive social assistance (prop.)", labrfrc = "In labor force (prop.)", vacant = "Vacant properties (prop.)",
                 renters = "Renters (prop.)", yearblt = "Median year property built", rntbrdn = "Median rent burdened (prop.)")

table4215 <- tableby(type ~ popultn + hs_r_ls + poverty + ag_65_l + hispanc + black + family + foreign + wrkfrmh + 
                    lngcmmt + assstnc + labrfrc + vacant + renters + yearblt + rntbrdn, 
                    data = thezones[["Zone 4215"]], 
                    control = tableby.control(numeric.stats = c("meansd"), digits = 2), numeric.simplify = TRUE) # also show range with "range"
table4810 <- tableby(type ~ popultn + hs_r_ls + poverty + ag_65_l + hispanc + black + family + foreign + wrkfrmh + 
                     lngcmmt + assstnc + labrfrc + vacant + renters + yearblt + rntbrdn, 
                     data = thezones[["Zone 4810"]], 
                     control = tableby.control(numeric.stats = c("meansd"), digits = 2), numeric.simplify = TRUE) # also show range with "range"
table4216 <- tableby(type ~ popultn + hs_r_ls + poverty + ag_65_l + hispanc + black + family + foreign + wrkfrmh + 
                     lngcmmt + assstnc + labrfrc + vacant + renters + yearblt + rntbrdn, 
                     data = thezones[["Zone 4216"]], 
                     control = tableby.control(numeric.stats = c("meansd"), digits = 2), numeric.simplify = TRUE) # also show range with "range"
table4515_02 <- tableby(type ~ popultn + hs_r_ls + poverty + ag_65_l + hispanc + black + family + foreign + wrkfrmh + 
                     lngcmmt + assstnc + labrfrc + vacant + renters + yearblt + rntbrdn, 
                     data = thezones[["Zone 4515_02"]], 
                     control = tableby.control(numeric.stats = c("meansd"), digits = 2), numeric.simplify = TRUE) # also show range with "range"
table4528_01 <- tableby(type ~ popultn + hs_r_ls + poverty + ag_65_l + hispanc + black + family + foreign + wrkfrmh + 
                        lngcmmt + assstnc + labrfrc + vacant + renters + yearblt + rntbrdn, 
                        data = thezones[["Zone 4528_01"]], 
                        control = tableby.control(numeric.stats = c("meansd"), digits = 2), numeric.simplify = TRUE) # also show range with "range"
table4154_01 <- tableby(type ~ popultn + hs_r_ls + poverty + ag_65_l + hispanc + black + family + foreign + wrkfrmh + 
                        lngcmmt + assstnc + labrfrc + vacant + renters + yearblt + rntbrdn, 
                        data = thezones[["Zone 4154_01"]], 
                        control = tableby.control(numeric.stats = c("meansd"), digits = 2), numeric.simplify = TRUE) # also show range with "range"
table4218 <- tableby(type ~ popultn + hs_r_ls + poverty + ag_65_l + hispanc + black + family + foreign + wrkfrmh + 
                     lngcmmt + assstnc + labrfrc + vacant + renters + yearblt + rntbrdn, 
                     data = thezones[["Zone 4218"]], 
                     control = tableby.control(numeric.stats = c("meansd"), digits = 2), numeric.simplify = TRUE) # also show range with "range"
table4821 <- tableby(type ~ popultn + hs_r_ls + poverty + ag_65_l + hispanc + black + family + foreign + wrkfrmh + 
                     lngcmmt + assstnc + labrfrc + vacant + renters + yearblt + rntbrdn, 
                     data = thezones[["Zone 4821"]], 
                     control = tableby.control(numeric.stats = c("meansd"), digits = 2), numeric.simplify = TRUE) # also show range with "range"
table4514 <- tableby(type ~ popultn + hs_r_ls + poverty + ag_65_l + hispanc + black + family + foreign + wrkfrmh + 
                     lngcmmt + assstnc + labrfrc + vacant + renters + yearblt + rntbrdn, 
                     data = thezones[["Zone 4514"]], 
                     control = tableby.control(numeric.stats = c("meansd"), digits = 2), numeric.simplify = TRUE) # also show range with "range"

summary(table4215, text = TRUE, labelTranslations = mylabels)
summary(table4810, text = TRUE, labelTranslations = mylabels)
summary(table4216, text = TRUE, labelTranslations = mylabels)
summary(table4515_02, text = TRUE, labelTranslations = mylabels)
summary(table4528_01, text = TRUE, labelTranslations = mylabels)
summary(table4154_01, text = TRUE, labelTranslations = mylabels)
summary(table4218, text = TRUE, labelTranslations = mylabels)
summary(table4821, text = TRUE, labelTranslations = mylabels)
summary(table4514, text = TRUE, labelTranslations = mylabels)

