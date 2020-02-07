library(readr)
library(dplyr)
library(janitor)
library(tigris)
library(sf)
library(ggthemes)
library(ggplot2)
library(purrr)
library(viridis)
library(magick)


# Import, clean
eviction <- read_csv("data/original/eviction/eviction-block-groups.csv", col_types = list(GEOID = "c"))
eviction <- clean_names(eviction)
names(eviction)

# Filter to useful data
# Standardized to 2010 geographies (supposedly), sociodemographic/economic data looks funny.
# Eviction and filing data seems to have consistent gaps 2000-2002 and 2007-2010.
eviction <- eviction %>% filter(parent_location == "Fairfax County, Virginia")
eviction <- eviction %>% select(geoid, year, name, renter_occupied_households, eviction_filings, evictions, eviction_rate, eviction_filing_rate, low_flag, imputed, subbed)

# Two block groups have a 200% eviction rate. Each has 1 renter-occupied HH but 2 or 3 evictions. 
# Topcoding these to the max eviction rate found in data, which is 30.53%.
eviction <- eviction %>% mutate(eviction_rate = ifelse(eviction_rate > 50, 30.53, eviction_rate))

# Get shapefiles
fairfax <- block_groups(state = 51, county = 059)
fairfax <- st_as_sf(fairfax)

# Join
eviction <- left_join(eviction, fairfax, by = c("geoid" = "GEOID"))
eviction <- st_as_sf(eviction)

evict03 <- eviction %>% filter(year == 2003)
evict04 <- eviction %>% filter(year == 2004)
evict05 <- eviction %>% filter(year == 2005)
evict06 <- eviction %>% filter(year == 2006)
evict11 <- eviction %>% filter(year == 2011)
evict12 <- eviction %>% filter(year == 2012)
evict13 <- eviction %>% filter(year == 2013)
evict14 <- eviction %>% filter(year == 2014)
evict15 <- eviction %>% filter(year == 2015)
evict16 <- eviction %>% filter(year == 2016)

#
# Prepare data and names ---------------------------------------------------------------------------------------------------------------------
#

dflist <- list(evict03, evict04, evict05, evict06, evict11, evict12, evict13, evict14, evict15, evict16)
dflist <- set_names(dflist, c("Eviction Rate, 2003", "Eviction Rate, 2004", "Eviction Rate, 2005", "Eviction Rate, 2006", 
                              "Eviction Rate, 2011", "Eviction Rate, 2012", "Eviction Rate, 2013", "Eviction Rate, 2014", 
                              "Eviction Rate, 2015", "Eviction Rate, 2016"))

dfnames <- list("Eviction Rate, 2003", "Eviction Rate, 2004", "Eviction Rate, 2005", "Eviction Rate, 2006", 
                "Eviction Rate, 2011", "Eviction Rate, 2012", "Eviction Rate, 2013", "Eviction Rate, 2014", 
                "Eviction Rate, 2015", "Eviction Rate, 2016")

savenames <- list("evrate-03", "evrate-04", "evrate-05", "evrate-06", 
                "evrate-11", "evrate-12", "evrate-13", "evrate-14", 
                "evrate-15", "evrate-16")

#
# Plot eviction rates ---------------------------------------------------------------------------------------------------------------------
#

# Get min & max for scale
max_rate <- sapply(dflist, function(x) max(x$eviction_rate, na.rm = TRUE)) %>% max()
min_rate <- sapply(dflist, function(x) min(x$eviction_rate, na.rm = TRUE)) %>% min()

# Function
plotevrate <- function(x, y) {
  ggplot(x) +
    geom_sf(aes(fill = eviction_rate), size = 0.1) +
    labs(title = paste("Block-Group Level", y, sep = " ")) +
    theme_map() + 
    theme(plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 11, face = "bold"),
          legend.text = element_text(size = 11))  +
    scale_fill_viridis_c(name = "Proportion", limits = c(min_rate, max_rate), 
                         breaks = seq(round(min_rate, 2), round(max_rate, 2), round((min_rate+max_rate)/4, 2)))
}

# Plot
plots_evrate <- map2(dflist, dfnames, ~plotevrate(.x, .y))

for (i in 1:length(plots_evrate)) {
  assign(paste("plot_evrate", i, sep = "_"), plots_evrate[[i]])
}

# Save
plotnames <- paste0("plot_", savenames, ".png")
plotnames

walk2(plotnames, plots_evrate, ~ggsave(path = "./docs/evictionplots/", device = "png", filename = .x, plot = .y, height = 7, width = 7))

# Animate
list.files(path = "./docs/evictionplots/", pattern = "plot_evrate", full.names = T) %>% 
  map(image_read) %>%
  image_join() %>%
  image_animate(fps = 0.5) %>%
  image_write("./docs/evictionplots/animation_evict.gif")
