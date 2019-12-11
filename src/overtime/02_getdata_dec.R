
# Notes
# dataset = "sf3" indicates census long form, "sf1" is short form, https://dusp.mit.edu/sites/dusp.mit.edu/files/attachments/publications/working_with_acs_R_v_2.0.pdf page 18 (UGH!)
# SF1 includes population tables (identified with a ''P'') and housing tables (identified with an ''H'') shown down to various levels of geography.
# https://www.census.gov/data/developers/data-sets/decennial-census.2000.html


#
# Find variables (note: this takes forever): 2000 -------------------------------------------------------------------------
#
 
allvars_long <- load_variables(year = 2000, dataset = "sf3", cache = TRUE)
View(allvars_long)

# Black
# Total pop: P006001
# Black: P006003

# Hispanic
# Total pop: P007001
# Hispanic: P007010

# Less than HS
# All 25+: P037001
# Male less HS: P037003, P037004, P037005, P037006, P037007, P037008, P037009, P037010
# Female less HS: P037020, P037021, P037022, P037023, P037024, P037025, P037026, P037027

# In poverty
# All population for whom poverty status is determined: P087001
# Income below poverty: 	P087002

# Single parent v1
# Total households: P010001
# Male HH no wife, has own kids: P010012
# Female HH no husband, has own kids: P010015

# Single parent v2
# Total families: P015001
# Male HH no wife, own kids: P015010
# Female HH no husband, own kids: P015016

# Single parent v3
# All own children under 18: P016001
# Male HH no wife: P016011
# Female HH no husband: P016019

# Select variables
sf3vars_2000 <- c("P037001", "P037003", "P037004", "P037005", "P037006", "P037007", 
                  "P037008", "P037009", "P037010", "P037020", "P037021", "P037022", 
                  "P037023", "P037024", "P037025", "P037026", "P037027", # less than HS 
                  "P006001", "P006003",                                  # black
                  "P007001", "P007010",                                  # hispanic
                  "P087001", "P087002",                                  # in poverty
                  "P016001", "P016011", "P016019")                       # single parent

# Get data
dec2000 <- get_decennial(geography = "tract", year = 2000, state = 51, county = 059, sumfile = "sf3", 
                         variables = sf3vars_2000, geometry = TRUE, keep_geo_vars = TRUE, output = "wide")

# Clean
dec2000 <- dec2000 %>% mutate(
  lesshs = (P037003 + P037004 + P037005 + P037006 + P037007 + P037008 + P037009 + P037010 + 
            P037020 + P037021 + P037022 + P037023 + P037024 + P037025 + P037026 + P037027) / P037001,
  hispanic = P007010 / P007001,
  black = P006003 / P006001,
  inpoverty = P087002 / P087001,
  singleparent = (P016011 + P016019) / P016001)


#
# Find variables (note: this takes forever): 1990 -------------------------------------------------------------------------
#


# Note: all tables are named "population subjects" and I have no idea what the denominators are....

allvars_long90 <- load_variables(year = 1990, dataset = "sf3", cache = TRUE)
View(allvars_long90)

# Persons total: P0010001
# Families total: P0040001
# Households total: P0050001

# Black: P0080002

# Hispanic
# Total: P0100001
# Hispanic by race: P0120006, P0120007, P0120008, P0120009, P0120010

# Total: presumably households, P0050001
# Male HH no wife, with child: P0190003
# Female HH no husband, with child: P0190005

# Total: presumably population,  P0010001
# Education: P0570001, P0570002 (less than 9th and 12 no diploma)
# Education 2: P0590001, P0590002 (less than 9th and 12 no diploma)

# Incime below poverty: P1170013 to P1170024
# Incime below poverty: P1200008 to P1200014

# Select variables
sf3vars_1990 <- c("P0010001", "P0040001", "P0050001",                                     # person, family, household total
                  "P0080002",                                                             # black,
                  "P0100001", "P0120006", "P0120007", "P0120008", "P0120009", "P0120010",             # hispanic
                  "P0190003", "P0190005",                                                 # single parent
                  "P0570001", "P0570002",                                                 # education
                  "P1200008", "P1200009", "P1200010", "P1200011", "P1200012", "P1200013", "P1200014") # income below poverty)              

# Get data
dec1990 <- get_decennial(geography = "tract", year = 1990, state = 51, county = 059, sumfile = "sf3", variables = sf3vars_1990, 
                         geometry = TRUE, keep_geo_vars = TRUE, output = "wide")


# Clean
dec1990 <- dec1990 %>% mutate(
  lesshs = (P0570001 + P0570002) / P0010001,
  hispanic = (P0120006 + P0120007 + P0120008 + P0120009 + P0120010) / P0100001,
  black = P0080002 / P0010001,
  inpoverty = (P1200008 + P1200009 + P1200010 + P1200011 + P1200012 + P1200013 + P1200014) / P0050001,
  singleparent = (P0190003 + P0190005) / P0050001)

#
# Alternatives --------------------------------------------------------------------------------------------
#

# Lookup
search_tablecontents(survey = "dec", years = 2000)

# Then you can look up labels with
acs.lookup(endyear = 1990, dataset = "sf3", keyword = "EDUCATION")
