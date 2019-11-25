library(readr)
library(dplyr)
library(tigris)
library(sf)
library(stringr)
library(tidyselect)


# Note: 01-read to read in the files. 

#
# Variables --------------------------------------------------------------------
#

# C000 Num Total number of jobs
# 
# CA01 Num Number of jobs for workers age 29 or younger16
# CA02 Num Number of jobs for workers age 30 to 5416
# CA03 Num Number of jobs for workers age 55 or older16
# 
# CE01 Num Number of jobs with earnings $1250/month or less
# CE02 Num Number of jobs with earnings $1251/month to $3333/month
# CE03 Num Number of jobs with earnings greater than $3333/month 
# 
# CR01 Num Number of jobs for workers with Race: White, Alone
# CR02 Num Number of jobs for workers with Race: Black or African American Alone
# CR03 Num Number of jobs for workers with Race: American Indian or Alaska Native Alone
# CR04 Num Number of jobs for workers with Race: Asian Alone
# CR05 Num Number of jobs for workers with Race: Native Hawaiian or Other Pacific Islander Alone
# CR07 Num Number of jobs for workers with Race: Two or More Race Groups
# CT01 Num Number of jobs for workers with Ethnicity: Not Hispanic or Latino
# CT02 Num Number of jobs for workers with Ethnicity: Hispanic or Latino
# 
# CD01 Num Number of jobs for workers with Educational Attainment: Less than high school
# CD02 Num Number of jobs for workers with Educational Attainment: High school or equivalent, no college
# CD03 Num Number of jobs for workers with Educational Attainment: Some college or Associate degree
# CD04 Num Number of jobs for workers with Educational Attainment: Bachelor's degree or advanced degree
# 
# CS01 Num Number of jobs for workers with Sex: Male
# CS02 Num Number of jobs for workers with Sex: Female

# CFA01 Num Number of jobs for workers at firms with Firm Age: 0-1 Years
# CFA02 Num Number of jobs for workers at firms with Firm Age: 2-3 Years
# CFA03 Num Number of jobs for workers at firms with Firm Age: 4-5 Years
# CFA04 Num Number of jobs for workers at firms with Firm Age: 6-10 Years
# CFA05 Num Number of jobs for workers at firms with Firm Age: 11+ Years
# CFS01 Num Number of jobs for workers at firms with Firm Size: 0-19 Employees
# CFS02 Num Number of jobs for workers at firms with Firm Size: 20-49 Employees
# CFS03 Num Number of jobs for workers at firms with Firm Size: 50-249 Employees
# CFS04 Num Number of jobs for workers at firms with Firm Size: 250-499 Employees
# CFS05 Num Number of jobs for workers at firms with Firm Size: 500+ Employees


#
# Filter to Fairfax county ---------------------------------------------------------
#

wac09_ffx <- left_join(xwalk_ffx, wac09, by = c("tabblk2010" = "w_geocode"))
wac17_ffx <- left_join(xwalk_ffx, wac17, by = c("tabblk2010" = "w_geocode"))


#
# WAC 2009 by tract ---------------------------------------------------------
#

# Create variables
wac09_ffx_tract <- wac09_ffx %>% arrange(trct) %>%
                                 group_by(trct) %>% mutate(job09_all = sum(C000, na.rm = TRUE),
                                                           job09_age29less = sum(CA01, na.rm = TRUE)/job09_all,
                                                           job09_age3054 = sum(CA02, na.rm = TRUE)/job09_all,
                                                           job09_age55ovr = sum(CA03, na.rm = TRUE)/job09_all,
                                                           job09_earn1250less = sum(CE01, na.rm = TRUE)/job09_all,
                                                           job09_earn12513333 = sum(CE01, na.rm = TRUE)/job09_all,
                                                           job09_earn3334ovr = sum(CE01, na.rm = TRUE)/job09_all,
                                                           job09_white = sum(CR01, na.rm = TRUE)/job09_all,
                                                           job09_black = sum(CR02, na.rm = TRUE)/job09_all,
                                                           job09_native = sum(CR03, na.rm = TRUE)/job09_all,
                                                           job09_asian = sum(CR04, na.rm = TRUE)/job09_all,
                                                           job09_pacific = sum(CR05, na.rm = TRUE)/job09_all,
                                                           job09_multirace = sum(CR07, na.rm = TRUE)/job09_all,
                                                           job09_nonhisp = sum(CT01, na.rm = TRUE)/job09_all,
                                                           job09_hisp = sum(CT02, na.rm = TRUE)/job09_all,
                                                           job09_educlesshs = sum(CD01, na.rm = TRUE)/job09_all,
                                                           job09_educhsequiv = sum(CD02, na.rm = TRUE)/job09_all,
                                                           job09_educsomecol = sum(CD03, na.rm = TRUE)/job09_all,
                                                           job09_educba = sum(CD04, na.rm = TRUE)/job09_all,
                                                           job09_male = sum(CS01, na.rm = TRUE)/job09_all,
                                                           job09_female = sum(CS02, na.rm = TRUE)/job09_all) %>%
                                 ungroup()

# Select relevant columns 
wac09_ffx_tract <- wac09_ffx_tract %>% select(cty, ctyname, trct, trctname, starts_with("job09_"))

# Get one row per tract
wac09_ffx_tract <- wac09_ffx_tract %>% group_by(trct) %>% slice(1)
  

#
# WAC 2017 by tract ---------------------------------------------------------
#

# Calculate proportion of total jobs by category variables
wac17_ffx_tract <- wac17_ffx %>% arrange(trct) %>%
                                 group_by(trct) %>% mutate(job17_all = sum(C000, na.rm = TRUE),
                                                           job17_age29less = sum(CA01, na.rm = TRUE)/job17_all,
                                                           job17_age3054 = sum(CA02, na.rm = TRUE)/job17_all,
                                                           job17_age55ovr = sum(CA03, na.rm = TRUE)/job17_all,
                                                           job17_earn1250less = sum(CE01, na.rm = TRUE)/job17_all,
                                                           job17_earn12513333 = sum(CE01, na.rm = TRUE)/job17_all,
                                                           job17_earn3334ovr = sum(CE01, na.rm = TRUE)/job17_all,
                                                           job17_white = sum(CR01, na.rm = TRUE)/job17_all,
                                                           job17_black = sum(CR02, na.rm = TRUE)/job17_all,
                                                           job17_native = sum(CR03, na.rm = TRUE)/job17_all,
                                                           job17_asian = sum(CR04, na.rm = TRUE)/job17_all,
                                                           job17_pacific = sum(CR05, na.rm = TRUE)/job17_all,
                                                           job17_multirace = sum(CR07, na.rm = TRUE)/job17_all,
                                                           job17_nonhisp = sum(CT01, na.rm = TRUE)/job17_all,
                                                           job17_hisp = sum(CT02, na.rm = TRUE)/job17_all,
                                                           job17_educlesshs = sum(CD01, na.rm = TRUE)/job17_all,
                                                           job17_educhsequiv = sum(CD02, na.rm = TRUE)/job17_all,
                                                           job17_educsomecol = sum(CD03, na.rm = TRUE)/job17_all,
                                                           job17_educba = sum(CD04, na.rm = TRUE)/job17_all,
                                                           job17_male = sum(CS01, na.rm = TRUE)/job17_all,
                                                           job17_female = sum(CS02, na.rm = TRUE)/job17_all) %>%
                                  ungroup()

# Select relevant columns 
wac17_ffx_tract <- wac17_ffx_tract %>% select(cty, ctyname, trct, trctname, starts_with("job17_"))

# Get one row per tract
wac17_ffx_tract <- wac17_ffx_tract %>% group_by(trct) %>% slice(1)


#
# Get geography & join---------------------------------------------------------
#

# Get tract geography
ffxcounty <- tracts("VA", county = "059")
ffxcounty <- st_as_sf(ffxcounty)

head(ffxcounty)

# Join 2009 and 2017 data
wac0917 <- left_join(wac09_ffx_tract, wac17_ffx_tract, by = c("cty", "ctyname", "trct", "trctname"))

# Join data with geography
wac0917geo <- left_join(ffxcounty, wac0917, by = c("GEOID" = "trct"))

# Check CRS
st_crs(wac0917geo)
wac0917geo <- wac0917geo %>% st_transform("+proj=longlat +datum=WGS84")










