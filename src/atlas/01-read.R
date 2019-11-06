library(readr)
library(dplyr)
library(downloader)

#
# Get data & filter ----------------------------------------------------------------------------------------------------
#

# Unzip
unzip ("./data/original/atlas/tract_outcomes.zip", exdir = "./data/original/atlas/")

# Import (note outcomes will take a while)
hhinc_jail <- read_csv("./data/original/atlas/hhinc_jail.csv", progress = show_progress())
neighb_chars <- read_csv("./data/original/atlas/neighb_chars.csv", progress = show_progress())
outcomes <- read_csv("./data/original/atlas/tract_outcomes_early.csv", progress = show_progress(), col_names = TRUE, cols(czname = "c", .default = col_double()))

# Filter to Fairfax County and Fairfax City
# Files contain 2010 county FIPS codes. Reference:
# https://www.census.gov/geographies/reference-maps/2010/geo/county-wallmaps-2010.html
# https://www2.census.gov/geo/maps/general_ref/us_base/stco2010/USstcou2010_wallmap.pdf
# Fairfax = 059 county code
# Fairfax City = 600 "county" code

hhinc_jail_ffx <- hhinc_jail %>% filter(state == 51 & (county == 059 | county == 600))
neighb_chars_ffx <- neighb_chars %>% filter(state == 51 & (county == 059 | county == 600))
outcomes_ffx <- outcomes %>% filter(state == 51 & (county == 059 | county == 600))

# Clean up
remove(hhinc_jail)
remove(neighb_chars)
remove(outcomes)


#
# Write out ----------------------------------------------------------------------------------------------------
#

write_csv(hhinc_jail_ffx, "./data/working/atlas/hhinc_jail_ffx.csv", col_names = TRUE, append = FALSE)
write_csv(neighb_chars_ffx, "./data/working/atlas/neighb_chars_ffx.csv", col_names = TRUE, append = FALSE)
write_csv(outcomes_ffx, "./data/working/atlas/outcomes_ffx.csv", col_names = TRUE, append = FALSE)
