library(RPostgreSQL)
library(dplyr)


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
cl_ffx_01 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_01 WHERE fips_code = '51059'") # 374,831
cl_ffx_02 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_02 WHERE fips_code = '51059'") # 0
cl_ffx_03 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_03 WHERE fips_code = '51059'") # 0
cl_ffx_04 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_04 WHERE fips_code = '51059'") # 0
cl_ffx_05 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_05 WHERE fips_code = '51059'") # 0
cl_ffx_06 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_06 WHERE fips_code = '51059'") # 0
cl_ffx_07 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_07 WHERE fips_code = '51059'") # 0
cl_ffx_08 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_08 WHERE fips_code = '51059'") # 0
cl_ffx_09 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_09 WHERE fips_code = '51059'") # 0
# Tables 02-09 seem to be empty. Neil says Aaron is having issues with upload so these are not up. 

# Get data from _1_51 files ("latest tax data" files split by state, 51 is VA), VA FIPS is 51059
cl_ffx_1 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_1_51 WHERE fips_code = '51059'") # 374,831

# Get data from _1 files ("latest tax data" files), VA FIPS is 51059
cl_ffx_1_only <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_1 WHERE fips_code = '51059'")

# Get data from _2_51 files ("latest tax data + property characteristics" files split by state, 51 is VA), VA FIPS is 51059
cl_ffx_2 <- dbGetQuery(conn, "SELECT * FROM corelogic_sdad.tax_hist_2_51 WHERE fips_code = '51059'") # 375410

# Equivalent number of rows in _01 (dump), _1, (all latest) and _1_51 (latest for VA). 
# Extra rows in _2 (latest with property) and _2_15 (latest with property for VA)


#
# Check years ---------------------------------------------------------------------------------------------
#

# Check years
table(cl_ffx_01$tax_year, useNA = "always")          # all 2017
table(cl_ffx_01$assessed_year, useNA = "always")     # all 2017

table(cl_ffx_1$tax_year, useNA = "always")           # all 2017
table(cl_ffx_1$assessed_year, useNA = "always")      # all 2017

table(cl_ffx_1_only$tax_year, useNA = "always")      # all 2017
table(cl_ffx_1_only$assessed_year, useNA = "always") # all 2017

table(cl_ffx_2$tax_year, useNA = "always")           # all 2018
table(cl_ffx_2$assessed_year, useNA = "always")      # all 2018

# "Latest with property" (more rows) is all 2018. The other two files (equivalent rows) are all 2017.
 

#
# Disconnect ---------------------------------------------------------------------------------------------
#

dbDisconnect(conn)
rm(conn)


