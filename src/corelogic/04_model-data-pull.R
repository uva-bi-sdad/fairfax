library(RPostgreSQL)
library(dplyr)
library(readr)

#
# Connect to DB ---------------------------------------------------------------------------------------------
#

conn <- dbConnect(drv = PostgreSQL(), dbname = "sdad", 
                  host = "10.250.124.195", 
                  port = 5432, 
                  user = Sys.getenv("db_userid"), 
                  password = Sys.getenv("db_pwd"))

#
# Get Fairfax County data from the available tables for # obs cross-check -----------------------------------------------------------
#

# Get data from the 01-09 files (dump split into tables); Fairfax County, VA FIPS is 51059
#cl_ffx_01 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_01 WHERE fips_code = '51059'") # 374,831
cl_ffx_02 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_02 WHERE fips_code = '51059'") # 374,838
#cl_ffx_03 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_03 WHERE fips_code = '51059'") # 373,741
#cl_ffx_04 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_04 WHERE fips_code = '51059'") # 373,276
#cl_ffx_05 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_05 WHERE fips_code = '51059'") # 372,725
#cl_ffx_06 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_06 WHERE fips_code = '51059'") # 372,497
cl_ffx_07 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_07 WHERE fips_code = '51059'") # 371,930
#cl_ffx_08 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_08 WHERE fips_code = '51059'") # 371,624
#cl_ffx_09 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_09 WHERE fips_code = '51059'") # 372,319


dbDisconnect(conn)
rm(conn)

#
#  Create Fairfax Dataframe at the tract level -------------------------------------------------------------------
#  (Organize data from 2010 (cl_ffx_08) and 2016 (cl_ffx_02) with variables that we need for Fairfax model) 
#  2010 data appeared to be by tracts from the 2000 census, so replacing 2010 with 2011 data (cl_ffx_07).

cl_11 <- cl_ffx_07
cl_16 <- cl_ffx_02

#
#  Geography: FIPS state, county and census track
#  GEOID contains state (2), county (3), tract (6), block group (1) OR block (4) information

cl_11$STATEFP <- substr(cl_11$fips_code, start=1, stop=2)
cl_11$COUNTYFP <- substr(cl_11$fips_code, start=3, stop=5) 
cl_11$TRACTCE <- substr(cl_11$census_tract, start=1, stop=6)
cl_11$GEOID <- paste(cl_11$fips_code, substr(cl_11$census_tract, start=1, stop=6), sep="")  

cl_16$STATEFP <- substr(cl_16$fips_code, start=1, stop=2)
cl_16$COUNTYFP <- substr(cl_16$fips_code, start=3, stop=5) 
cl_16$TRACTCE <- substr(cl_16$census_tract, start=1, stop=6)
cl_16$GEOID <- paste(cl_16$fips_code, substr(cl_16$census_tract, start=1, stop=6), sep="")


#
# Housing Value: calculated using the Total Calculated Value (var: total_value_calculated)
# (The "TOTAL" (i.e., Land + Improvement) Value closest to current market value used for assessment by county 
# or local taxing authorities.)
#

# Dollar amounts adjusted to 2018 dollars using BLS CPI.  
#   Average annual CPIS (https://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/): 

#cpi_2010 <- 218.056 
cpi_2011 <- 224.939
cpi_2016 <- 240.007 
cpi_2018 <- 251.107 

cl_11 <- cl_11 %>%
          mutate(tvc_2018_dollars = as.numeric(total_value_calculated)*(cpi_2018/cpi_2011))

cl_16 <- cl_16 %>%
          mutate(tvc_2018_dollars = as.numeric(total_value_calculated)*(cpi_2018/cpi_2016))


#
# Property Counts: binary variables for 1) retail & service buildings, 2) vacant buildings, 3) industrial buildings
#


cl_11$bin_retail <- as.integer(cl_11$property_indicator == '25' | cl_11$property_indicator == '26' |
                               cl_11$property_indicator == '29')
cl_11$bin_vacant <- as.integer(cl_11$property_indicator == '80')
cl_11$bin_industrial <- as.integer(cl_11$property_indicator >= '50' & cl_11$property_indicator <= '69')

cl_16$bin_retail <- as.integer(cl_16$property_indicator == '25' | cl_16$property_indicator == '26' |
                                 cl_16$property_indicator == '29')
cl_16$bin_vacant <- as.integer(cl_16$property_indicator == '80')
cl_16$bin_industrial <- as.integer(cl_16$property_indicator >= '50' & cl_16$property_indicator <= '69')


#
# Renovations: binary variable for if property was renovated or not (if effective year built != NA)
#

cl_11$reno <- as.integer(!is.na(cl_11$effective_year_built))
cl_16$reno <- as.integer(!is.na(cl_16$effective_year_built))

#
# Aggregate Results by census tract
#
  
# =====================================================
# Create the aggregated 2011 dataframe

ffx_data_11 <- cl_11 %>%
                  select(STATEFP, COUNTYFP, TRACTCE, GEOID, tvc_2018_dollars, 
                         bin_retail, bin_vacant, bin_industrial, reno) %>%
                  group_by(STATEFP, COUNTYFP, TRACTCE, GEOID) %>%
                  summarise(num_retail11 = sum(bin_retail, na.rm = TRUE),
                            num_vac11 = sum(bin_vacant, na.rm = TRUE),
                            num_ind11 = sum(bin_industrial, na.rm = TRUE),
                            num_reno11 = sum(reno, na.rm = TRUE),
                            num_properties11 = n()) %>%
                            mutate(percent_reno11 = num_reno11/num_properties11) %>%
                  ungroup()


# Find median home price for residential homes (family residence and condos) 
# using total value calculated that has been adjusted to 2018 dollars

res_median_home_price_11 <- cl_11 %>%
                              select(TRACTCE, property_indicator, tvc_2018_dollars) %>%
                              filter(property_indicator == '10' | property_indicator == '11') %>%  
                              group_by(TRACTCE) %>%
                              summarise(med_reshouse11 = median(tvc_2018_dollars, na.rm=TRUE),
                                        num_res11 = n(),
                                        num_NA_res_tvc = sum(is.na(tvc_2018_dollars))) %>%
                              mutate(percent_NA_res_tvc = num_NA_res_tvc/num_res11) %>%
                              ungroup()


# Join the two 2010 dataframes created

ffx_2011 <- merge(x=ffx_data_11, y=res_median_home_price_11[1:3], by="TRACTCE",all=TRUE)


# ===================================================
# Create the aggregated 2016 dataframe
  
ffx_data_16 <- cl_16 %>%
                select(STATEFP, COUNTYFP, TRACTCE, GEOID, tvc_2018_dollars, 
                        bin_retail, bin_vacant, bin_industrial, reno) %>%
                group_by(STATEFP, COUNTYFP, TRACTCE, GEOID) %>%
                summarise(num_retail16 = sum(bin_retail, na.rm = TRUE),
                          num_vac16 = sum(bin_vacant, na.rm = TRUE),
                          num_ind16 = sum(bin_industrial, na.rm = TRUE),
                          num_reno16 = sum(reno, na.rm = TRUE),
                          num_properties16 = n()) %>%
                mutate(percent_reno16 = num_reno16/num_properties16) %>%
                ungroup()


# Find median home price for residential homes (family residence and condos) 
# using total value calculated that has been adjusted to 2018 dollars

res_median_home_price_16 <- cl_16 %>%
                            select(TRACTCE, property_indicator, tvc_2018_dollars) %>%
                            filter(property_indicator == '10' | property_indicator == '11') %>%  
                            group_by(TRACTCE) %>%
                            summarise(med_reshouse16 = median(tvc_2018_dollars, na.rm=TRUE),
                                      num_res16 = n(),
                                      num_NA_res_tvc = sum(is.na(tvc_2018_dollars))) %>%
                            mutate(percent_NA_res_tvc = num_NA_res_tvc/num_res16) %>%
                            ungroup()


# Join the two 2016 dataframes created

ffx_2016 <- merge(x=ffx_data_16, y=res_median_home_price_16[1:3], by="TRACTCE",all=TRUE)


# =========================================
# Create Fairfax model dataframe from the 2011 and 2016 data

ffx_11_16 <- merge(x=ffx_2011, y=ffx_2016, by=c("STATEFP", "COUNTYFP", "TRACTCE", "GEOID"),all=TRUE)


# add absolute and relative change columns: 2016 - 2011

ffx_11_16$chg_abs_reshouse11_16 <- ffx_11_16$med_reshouse16 - ffx_11_16$med_reshouse11
ffx_11_16$chg_rel_reshouse11_16 <- ffx_11_16$chg_abs_reshouse11_16/ffx_11_16$med_reshouse11

ffx_11_16$cgh_abs_retail11_16 <- ffx_11_16$num_retail16 - ffx_11_16$num_retail11
ffx_11_16$cgh_rel_retail11_16 <- ffx_11_16$cgh_abs_retail11_16/ffx_11_16$num_retail11

ffx_11_16$cgh_abs_vac11_16 <- ffx_11_16$num_vac16 - ffx_11_16$num_vac11
ffx_11_16$cgh_rel_vac11_16 <- ffx_11_16$cgh_abs_vac11_16/ffx_11_16$num_vac11

ffx_11_16$cgh_abs_ind11_16 <- ffx_11_16$num_ind16 - ffx_11_16$num_ind11
ffx_11_16$cgh_rel_ind11_16 <- ffx_11_16$cgh_abs_ind11_16/ffx_11_16$num_ind11











