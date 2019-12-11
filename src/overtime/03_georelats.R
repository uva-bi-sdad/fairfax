library(readr)
library(dplyr)


#
# Geography changes ------------------------------------------------------------------------------------------------------------
#

# Census Tract Relationship Files
# https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.html

# 2010 for VA: https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/va51trf.txt
# 2010 record layout: https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2010-census-tract-record-layout.html
# 2010 record header: https://www2.census.gov/geo/docs/maps-data/data/rel/trfheader.txt

va2010relat <- url("https://www2.census.gov/geo/docs/maps-data/data/rel/trf_txt/va51trf.txt")
va2010head <- url("https://www2.census.gov/geo/docs/maps-data/data/rel/trfheader.txt")

va2010relat <- read_csv(va2010relat, col_names = FALSE)
va2010head <- read_csv(va2010head, col_names = FALSE)

va2010 <- rbind(va2010head, va2010relat)
colnames(va2010) <- va2010 [1, ]
va2010 <- va2010[-1, ] 

va2010ffx <- va2010 %>% filter(COUNTY10 == "059")

# 2000 for VA: https://www2.census.gov/geo/relfiles/tract/va/va51pop.txt
# 2000 record layout: https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2000-tract-relationship-record-layout.html
# 2000 record header: https://www.census.gov/programs-surveys/geography/technical-documentation/records-layout/2000-tract-relationship-record-layout.html

va2000relat <- url("https://www2.census.gov/geo/relfiles/tract/va/va51pop.txt")
va2000relat <- read.fwf(va2000relat, widths = c(2, 3, 4, 2, 1, 9, 4, 2, 3, 4, 2, 1, 9, 4, 9, 14, 2, 60), header = FALSE, colClasses = "character")

va2000names <- c("stateFIPS90", "countyFIPS90", "tract90base", "tract90suffix", "tract90partflag", "census00popfor90", "pct09tractpop", 
                 "stateFIPS00", "countyFIPS00", "tract00base", "tract00suffix", "tract00partflag", "census00popfor00", "pct00tractpop", 
                 "areapop00", "landareasqmeter", "stateabbrev", "countyname")
colnames(va2000relat) <- va2000names