library(readxl)
library(janitor)
library(noncensus)
library(dplyr)

# Programs/Delegates
# The Federal Head Start and Early Head Start grant is awarded to the Fairfax County Board of Supervisors. 
# Fairfax County Public Schools and Higher Horizons are delegate agencies which manage and operate their 
# individual programs. Services are provided in family homes, family child care provider homes and in classrooms.
# https://www.fairfaxcounty.gov/office-for-children/head-start/additional-information


#
# Read in and clean names --------------------------------------------------------
#


# Two-level headers in program files (A, B, C). Skip the first (descriptive) header and retain only variable reference number. The number can be matched
# to description using the program details sheet.

pir19a <- read_excel("./data/original/headstart/pir_export_2019.xlsx", sheet = "Section A", 
                        col_names = TRUE, skip = 1, trim_ws = TRUE, progress = TRUE) %>% clean_names()

pir19b <- read_excel("./data/original/headstart/pir_export_2019.xlsx", sheet = "Section B", 
                        col_names = TRUE, skip = 1, trim_ws = TRUE, guess_max = 3400, progress = TRUE) %>% clean_names()

pir19c <- read_excel("./data/original/headstart/pir_export_2019.xlsx", sheet = "Section C", 
                        col_names = TRUE, skip = 1, trim_ws = TRUE, guess_max = 3400, progress = TRUE) %>% clean_names()

pir19prog <- read_excel("./data/original/headstart/pir_export_2019.xlsx", sheet = "Program Details", 
                        col_names = TRUE, trim_ws = TRUE, progress = TRUE) %>% clean_names()

pir19ref <- read_excel("./data/original/headstart/pir_export_2019.xlsx", sheet = "Reference", 
                          col_names = TRUE,  trim_ws = TRUE, progress = TRUE) %>% clean_names()

pir18a <- read_excel("./data/original/headstart/pir_export_2018.xlsx", sheet = "Section A", 
                     col_names = TRUE, skip = 1, trim_ws = TRUE, progress = TRUE) %>% clean_names()

pir18b <- read_excel("./data/original/headstart/pir_export_2018.xlsx", sheet = "Section B", 
                     col_names = TRUE, skip = 1, trim_ws = TRUE, guess_max = 3400, progress = TRUE) %>% clean_names()

pir18c <- read_excel("./data/original/headstart/pir_export_2018.xlsx", sheet = "Section C", 
                     col_names = TRUE, skip = 1, trim_ws = TRUE, guess_max = 3400, progress = TRUE) %>% clean_names()

pir18prog <- read_excel("./data/original/headstart/pir_export_2018.xlsx", sheet = "Program Details", 
                        col_names = TRUE, trim_ws = TRUE, progress = TRUE) %>% clean_names()

pir18ref <- read_excel("./data/original/headstart/pir_export_2018.xlsx", sheet = "Reference", 
                       col_names = TRUE,  trim_ws = TRUE, progress = TRUE) %>% clean_names()

#
# Get Fairfax ZIP codes and filter data ------------------------------------------
#

# Alternatively, use matches from 2014 ZIPs and 2016 Census counties from the Missouri Census Data Center http://mcdc.missouri.edu/applications/ (original/zips).

data(zip_codes)
ffxzip <- zip_codes %>% filter(fips == 51059) %>% select(zip, fips)

# Join to keep only Fairfax County
ffpir19a <- inner_join(ffxzip, pir19a, by = c("zip" = "zip_code"))
ffpir19b <- inner_join(ffxzip, pir19b, by = c("zip" = "zip_code"))
ffpir19c <- inner_join(ffxzip, pir19c, by = c("zip" = "zip_code"))

ffpir18a <- inner_join(ffxzip, pir18a, by = c("zip" = "zip_code"))
ffpir18b <- inner_join(ffxzip, pir18b, by = c("zip" = "zip_code"))
ffpir18c <- inner_join(ffxzip, pir18c, by = c("zip" = "zip_code"))



A.5 Home-based Option
A.6 Combination Option
A.7 Family Child Care Option


A.13.a Less than 1 Year Old
A.13.b 1 Year Old
A.13.c 2 Years Old
A.13.d 3 Years Old
A.13.e 4 Years Old
A.13.f 5 Years and Older
A.15 Total Cumulative Enrollment
