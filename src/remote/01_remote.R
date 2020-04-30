library(tidycensus)
library(acs)
library(dplyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(viridis)

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key


#
# Select variables -------------------------------------------------------------------------------------------------------------
#

# Digital connectivity
#a) percent households with no internet access or relying solely on cellular data to access the internet (Int1):
#b) percent households with no computing devices or relying solely on mobile devices to access the internet (Int2); 
#c) percent population with access to no providers or access to up to 10/1 advertised speeds only (Int3).

# a)
# B28002 PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD
# _001 (total), _003 (only dial up), _006 (cellular data plan with no other type of internet subscription), _010 (only satellite), _013 (no internet access)

# B28002_001
# B28002_003
# B28002_006
# B28002_010
# B28002_013

# b)
# B28007 LABOR FORCE STATUS BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD 
# _002 (total in the civilian labor force: employed and unemployed), _008 (no computer employed) + _014 (no computer unemployed)

# B28007_002
# B28007_008
# B28007_014

# Work
#a) Industry: industries that overall engage less in working from home. 
# occupations: share of workers employed in service, natural, construction, maintenance, production, transportation, material moving, 
#             and military specific occupations (Occ) 
# b)industry: share of workers employed in construction, manufacturing, wholesale, retail, transportation and warehousing, utilities, 
#           and government, including armed forces (Ind).

# b) B08124 MEANS OF TRANSPORTATION TO WORK BY OCCUPATION
# _001 (total) - _043 (already work at home), 
# _010, _012, _013, _014, _017, _019, _020, _021, _024, _026, _027, _028, _031, _033, _034, _035, _038, _040, _041, _042

# B08124_001
# B08124_043
# B08124_010
# B08124_012
# B08124_013
# B08124_014
# B08124_017
# B08124_019
# B08124_020
# B08124_021
# B08124_024
# B08124_026
# B08124_027
# B08124_028
# B08124_031
# B08124_033
# B08124_034
# B08124_035
# B08124_038
# B08124_040
# B08124_041
# B08124_042

# a) B08126 MEANS OF TRANSPORTATION TO WORK BY INDUSTRY 
# _001 (total) - _091 (already work at home)
# B08126_001
# B08126_091
# B08126_018
# B08126_019
# B08126_020
# B08126_021
# B08126_022
# B08126_029
# B08126_030
# B08126_033
# B08126_034
# B08126_035
# B08126_036
# B08126_037
# B08126_044
# B08126_045
# B08126_048
# B08126_049
# B08126_050
# B08126_051
# B08126_052
# B08126_059
# B08126_060
# B08126_063
# B08126_064
# B08126_065
# B08126_066
# B08126_067
# B08126_074
# B08126_075
# B08126_078
# B08126_079
# B08126_080
# B08126_081
# B08126_082
# B08126_089
# B08126_090

# Select variables
vars <- c(
  # presence and types of internet
  "B28002_001", "B28002_003", "B28002_006", "B28002_010", "B28002_013",
  # presence of computer
  "B28007_002", "B28007_008", "B28007_014",
  # occupations
  "B08124_001", "B08124_043", "B08124_010", "B08124_012", "B08124_013", "B08124_014", 
  "B08124_017", "B08124_019", "B08124_020", "B08124_021", "B08124_024", "B08124_026", 
  "B08124_027", "B08124_028", "B08124_031", "B08124_033", "B08124_034", "B08124_035",
  "B08124_038", "B08124_040", "B08124_041", "B08124_042",
  # industries
  "B08126_001", "B08126_091", "B08126_018", "B08126_019", "B08126_020", "B08126_021", 
  "B08126_022", "B08126_029", "B08126_030", "B08126_033", "B08126_034", "B08126_035", "B08126_036", "B08126_037", 
  "B08126_044", "B08126_045", "B08126_048", "B08126_049", "B08126_050", "B08126_051", 
  "B08126_052", "B08126_059", "B08126_060", "B08126_063", "B08126_064", "B08126_065", 
  "B08126_066", "B08126_067", "B08126_074", "B08126_075", "B08126_078", "B08126_079", 
  "B08126_080", "B08126_081", "B08126_082", "B08126_089", "B08126_090"
)                 


#
# Get variables for counties --------------------------------------------------------------------------------------
#

acsdata <- get_acs(geography = "tract", county = 059, state = 51, variables = vars, year = 2018, survey = "acs5", 
                   cache_table = TRUE, output = "wide", geometry = TRUE, keep_geo_vars = TRUE)


#
# Calculate --------------------------------------------------------------------------------------
#

data <- acsdata %>% mutate(
  STATEFP = STATEFP, 
  COUNTYFP = COUNTYFP, 
  AFFGEOID = AFFGEOID, 
  GEOID = GEOID, 
  LSAD = LSAD, 
  NAME.x = NAME.x, 
  ALAND = ALAND, 
  AWATER = AWATER, 
  geometry = geometry,
  # internet access (only dial up/only cellular/only satellite/none : total)
  internet = (B28002_003E + B28002_006E + B28002_010E + B28002_013E) / B28002_001E,
  # presence of computer for those in labor force (no computer employed + no computer unemployed : total in labor force)
  computer = (B28007_008E + B28007_014E) / B28007_002E,
  # occupations (not-wfh-friendly occupations / total occupations that don't already WFH)
  occup = (B08124_010E + B08124_012E + B08124_013E + B08124_014E + B08124_017E +
             B08124_019E + B08124_020E + B08124_021E + B08124_024E + B08124_026E +
             B08124_027E + B08124_028E + B08124_031E + B08124_033E + B08124_034E +
             B08124_035E + B08124_038E + B08124_040E + B08124_041E + B08124_042E) / (B08124_001E - B08124_043E),
  # industries (not-wfh-friendly industries / total in industries that don't already WFH)
  industr = (B08126_018E + B08126_019E + B08126_020E + B08126_021E + B08126_022E + B08126_029E +
               B08126_030E + B08126_033E + B08126_034E + B08126_035E + B08126_036E + B08126_037E +
               B08126_044E + B08126_045E + B08126_048E + B08126_049E + B08126_050E + B08126_051E +
               B08126_052E + B08126_059E + B08126_060E + B08126_063E + B08126_064E + B08126_065E +
               B08126_066E + B08126_067E + B08126_074E + B08126_075E + B08126_078E + B08126_079E +
               B08126_080E + B08126_081E + B08126_082E + B08126_089E + B08126_090E) / (B08126_001E - B08126_091E)
)


#
# Quintiles --------------------------------------------------------------------------------------
#

# Histograms
hist(data$internet)
hist(data$computer)
hist(data$occup)
hist(data$industr)

# Missingness
any(is.na(data))
test <- data %>% filter(is.na(computer) | is.na(internet) | is.na(occup) | is.na(industr))
# Looks like empty tracts. 3 tracts don't have data.
data <- data %>% filter(!is.na(computer), !is.na(internet), !is.na(occup), !is.na(industr))

# Find quintiles
data <- data %>% mutate(internetQuint = ntile(internet, 5),
                        computerQuint = ntile(computer, 5),
                        occupQuint = ntile(occup, 5),
                        industrQuint = ntile(industr, 5))

# Get quantile cutoffs
qint <- quantile(data$internet, prob = seq(0, 1, 0.2))
qcomp <- quantile(data$computer, prob = seq(0, 1, 0.2))
qoccup <- quantile(data$occup, prob = seq(0, 1, 0.2))
qind <- quantile(data$industr, prob = seq(0, 1, 0.2))

quintcuts <- bind_rows(qint, qcomp, qoccup, qind)
quintcuts$id <- c("Internet", "Computer", "Occupation", "Industry")
quintcuts


#
# Create variables --------------------------------------------------------------------------------------
#

# Counties were divided into five equal groups or quintiles
# Very High vulnerability:placed in 4/5th quintiles in        4 of the 4 variables
# High vulnerability:     placed in 4/5th quintiles in        3 of the 4 variables
# Moderate vulnerability: placed in the 4/5th quintiles in    2 of the 4 variables
# Low vulnerability:      placed in 4/5th quintiles in        1 of the 4 variables
# No vulnerability:       placed in 4/5th quintiles in        0 of the 4 variables

# Did they place in 4 or 5th quintile?
data <- data %>% mutate(internetTop = ifelse(internetQuint >= 4, 1, 0),
                        computerTop = ifelse(computerQuint >= 4, 1, 0),
                        occupTop = ifelse(occupQuint >= 4, 1, 0),
                        industrTop = ifelse(industrQuint >= 4, 1, 0),
                        scoreTop = internetTop + computerTop + occupTop + industrTop)

# Vulnerability  
data <- data %>% mutate(vulnerability = case_when(
  internetTop + computerTop + occupTop + industrTop == 4 ~ "Very High",
  internetTop + computerTop + occupTop + industrTop == 3 ~ "High",
  internetTop + computerTop + occupTop + industrTop == 2 ~ "Medium",       
  internetTop + computerTop + occupTop + industrTop == 1 ~ "Low",   
  internetTop + computerTop + occupTop + industrTop == 0 ~ "None"))
data$vulnerability <- factor(data$vulnerability, levels = c("None", "Low", "Medium", "High", "Very High"), ordered = TRUE)


#
# Map --------------------------------------------------------------------------------------
#

ggplot() +
  geom_sf(data = acsdata, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = data, size = 0.2, aes(fill = vulnerability)) +
  labs(title = "Fairfax County Tract-Level\nTelework Vulnerability", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.\n
                    \n
       Vulnerability calculated using information on % households with no broadband\n
       internet subscription, % persons in labor force with no computer available,\n
       % persons employed in telework unfriendly occupations not working remotely,\n
       and % persons employed in telework unfriendly industries not working remotely.\n
       \n
       Tracts are considered very high vulnerability if they placed in 4th or 5th quintile\n
       on all 4 indicators considered, high if on 3 indicators, medium if on 2, low if on\n
       1, and no vulnerability if they did not place in the 4th or 5th quintile on any\n
       indicators.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 9, face = "bold"),
        legend.text = element_text(size = 8),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis(name = "Vulnerability", guide = "legend", discrete = T)

