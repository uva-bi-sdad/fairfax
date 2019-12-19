library(readr)
library(dplyr)
library(janitor)
library(tigris)
library(sf)
library(ggplot2)
library(ggthemes)
library(viridis)

# LRS papers
# https://academic.oup.com/poq/article-abstract/81/1/144/2649123?redirectedFrom=fulltext
# https://www.census.gov/content/dam/Census/newsroom/press-kits/2018/jsm/jsm-presentation-planning-database-low-response.pdf

# Planning data documentation
# https://www.census.gov/topics/research/guidance/planning-databases.html
# https://www.census.gov/content/dam/Census/topics/research/2019_Block_Group_PDBDocumentation_V3.pdf

# Read in planning data
data <- read_csv("./data/original/mailreturn/pdb2019bgv3_us.csv") 
data <- data %>% filter(State == "51" & County == "059") %>% clean_names()

# Get Fairfax block group geography
ffx <- block_groups(state = 51, county = 059)
ffx <- st_as_sf(ffx)
plot(st_geometry(ffx))

# Check that all block groups are in both datasets
any(!is.element(data$gidbg, ffx$GEOID))

# Join data + geo
mailback <- left_join(data, ffx, by = c("gidbg" = "GEOID"))
mailback <- st_as_sf(mailback)

# Plot
# 2010 return rate
ggplot(mailback) + 
  geom_sf(aes(fill = mail_return_rate_cen_2010)) +
  labs(title = "Fairfax County block group-level Census 2010 mail return rate", fill = "Return rate (%)",
       caption = "The map shows the percentage of mail returns received out of the total number of valid occupied\nhousing units in the mailout/mailback address universe, which excludes deleted, vacant, or UAA units.\nBlock groups containing only housing units not considered valid and occupied units set to NA.\nSource: Census 2010.") +
  theme_map() +
  scale_fill_viridis_c(breaks = seq(40, 100, 20), limits = c(40, 100)) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 11))

# 2020 
ggplot(mailback) + 
  geom_sf(aes(fill = low_response_score)) +
  labs(title = "Fairfax County block group-level Census 2020 low response score predictions", fill = "Low response score",
       caption = "The low response score is a predictor of self-response propensity derived from modeling Census 2010 mail non-response rate.\nThe score ranges from 0 to 100, with higher scores indicating lower predicted self-response rates (Erdman & Bates, 2017).\nBlock groups containing only housing units not considered valid and occupied units set to NA.\nSource: Census 2010.") +
  theme_map() +
  scale_fill_viridis_c(limits = c(0, 50)) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 11))
