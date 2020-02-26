library(naniar)
library(dplyr)

# Checking completeness & quality of data from the "full" tables (corelogic_sdad.tax_hist_01 through corelogic_sdad.tax_hist_0, 
# which correspond to tax/assessed years 2017 to 2009 in descending order).

options(scipen = 999)


#
# cl_ffx_01: 2017 data ----------------------------------------------------------------------
#

table(is.na(cl_ffx_01$assessed_year) & is.na(cl_ffx_01$tax_year)) # 7576 have neither tax nor assessed year info

cl_ffx_01 %>%
  select(fips_code, census_tract, # location
         land_use, county_use_1, # land use
         zoning, property_indicator,
         total_value_calculated, land_value_calculated, improvement_value_calculated, 
         total_value_calculated_ind, land_value_calculated_ind, improvement_value_calculated_ind, 
         assd_total_value, assd_land_value, assd_improvement_value, # assessed value
         mkt_total_value, mkt_land_value, mkt_improvement_value, # market value
         tax_amount, # tax
         tax_year, assessed_year, # tax and assessed years
         acres, land_square_footage, adjusted_gross_square_feet, universal_building_square_feet, building_square_feet_ind, # size
         building__square_feet, living_square_feet, # size
         year_built, 
         bedrooms, total_rooms, total_baths_calculated, full_baths,
         bldg_code, 
         construction_type, 
        stories_code, stories_number, number_of_units, units_number) %>%
  gg_miss_var()


#
# cl_ffx_02: 2016 data ----------------------------------------------------------------------
#

table(is.na(cl_ffx_02$assessed_year) & is.na(cl_ffx_02$tax_year)) # 7632 have neither tax nor assessed year info


#
# cl_ffx_03: 2015 data ----------------------------------------------------------------------
#

table(is.na(cl_ffx_03$assessed_year) & is.na(cl_ffx_03$tax_year)) # 7473 have neither tax nor assessed year info


#
# cl_ffx_04: 2014 data ----------------------------------------------------------------------
#

table(is.na(cl_ffx_04$assessed_year) & is.na(cl_ffx_04$tax_year)) # 13548 have neither tax nor assessed year info


#
# cl_ffx_05: 2013 data ----------------------------------------------------------------------
#

table(is.na(cl_ffx_05$assessed_year) & is.na(cl_ffx_05$tax_year)) # 13428 have neither tax nor assessed year info


#
# cl_ffx_06: 2012 data ----------------------------------------------------------------------
#

table(is.na(cl_ffx_06$assessed_year) & is.na(cl_ffx_06$tax_year)) # 13172 have neither tax nor assessed year info


#
# cl_ffx_07: 2011 data ----------------------------------------------------------------------
#

table(is.na(cl_ffx_07$assessed_year) & is.na(cl_ffx_07$tax_year)) # 12788 have neither tax nor assessed year info


#
# cl_ffx_08: 2010 data ----------------------------------------------------------------------
#

table(is.na(cl_ffx_08$assessed_year) & is.na(cl_ffx_08$tax_year)) # 12707 have neither tax nor assessed year info


#
# cl_ffx_09: 2009 data ----------------------------------------------------------------------
#

table(is.na(cl_ffx_09$assessed_year) & is.na(cl_ffx_09$tax_year)) # 12815 have neither tax nor assessed year info
