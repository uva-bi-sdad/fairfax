#
# MISSING IN 2018 --------------------------------------------------------------------------------------------
#

# Filter to tracts missing data from 14-18
miss18rent <- acs1418 %>% filter(is.na(B25064_001E)) %>% select(GEOID)
miss18house <- acs1418 %>% filter(is.na(B25077_001E)) %>% select(GEOID)
miss18income <- acs1418 %>% filter(is.na(B19013_001E)) %>% select(GEOID)

# Try to find data from 13-17 instead
get17rent <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25064_001", 
                   year = 2017, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get17house <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25077_001", 
                     year = 2017, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get17income <- get_acs(geography = "tract", state = 51, county = 059, variables = "B19013_001", 
                      year = 2017, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)

fill17rent <- left_join(miss18rent, get17rent, by = "GEOID") %>% rename(B25064_001E_rent17 = B25064_001E) %>% select(GEOID, B25064_001E_rent17) %>% st_set_geometry(NULL)
fill17house <- left_join(miss18house, get17house, by = "GEOID") %>% rename(B25077_001E_house17 = B25077_001E) %>% select(GEOID, B25077_001E_house17) %>% st_set_geometry(NULL)
fill17income <- left_join(miss18income, get17income, by = "GEOID") %>% rename(B19013_001E_income17 = B19013_001E) %>% select(GEOID, B19013_001E_income17) %>% st_set_geometry(NULL)

# Try to find data from 12-16 instead
miss17rent <- fill17rent %>% filter(is.na(B25064_001E_rent17)) %>% select(GEOID)
miss17house <- fill17house %>% filter(is.na(B25077_001E_house17)) %>% select(GEOID)
miss17income <- fill17income %>% filter(is.na(B19013_001E_income17)) %>% select(GEOID)

get16rent <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25064_001", 
                     year = 2016, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get16house <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25077_001", 
                      year = 2016, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get16income <- get_acs(geography = "tract", state = 51, county = 059, variables = "B19013_001", 
                       year = 2016, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)

fill16rent <- left_join(miss17rent, get16rent, by = "GEOID") %>% rename(B25064_001E_rent16 = B25064_001E) %>% select(GEOID, B25064_001E_rent16)
fill16house <- left_join(miss17house, get16house, by = "GEOID") %>% rename(B25077_001E_house16 = B25077_001E) %>% select(GEOID, B25077_001E_house16)
fill16income <- left_join(miss17income, get16income, by = "GEOID") %>% rename(B19013_001E_income16 = B19013_001E) %>% select(GEOID, B19013_001E_income16)

# Try to find data from 11-15 instead
miss16rent <- fill16rent %>% filter(is.na(B25064_001E_rent16)) %>% select(GEOID)
miss16house <- fill16house %>% filter(is.na(B25077_001E_house16)) %>% select(GEOID)
miss16income <- fill16income %>% filter(is.na(B19013_001E_income16)) %>% select(GEOID)

get15rent <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25064_001", 
                     year = 2015, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get15house <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25077_001", 
                      year = 2015, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get15income <- get_acs(geography = "tract", state = 51, county = 059, variables = "B19013_001", 
                       year = 2015, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)

fill15rent <- left_join(miss16rent, get15rent, by = "GEOID") %>% rename(B25064_001E_rent15 = B25064_001E) %>% select(GEOID, B25064_001E_rent15) 
fill15house <- left_join(miss16house, get15house, by = "GEOID") %>% rename(B25077_001E_house15 = B25077_001E) %>% select(GEOID, B25077_001E_house15) 
fill15income <- left_join(miss16income, get15income, by = "GEOID") %>% rename(B19013_001E_income15 = B19013_001E) %>% select(GEOID, B19013_001E_income15) 

# Try to find data from 10-14 instead
miss15rent <- fill15rent %>% filter(is.na(B25064_001E_rent15)) %>% select(GEOID)
miss15house <- fill15house %>% filter(is.na(B25077_001E_house15)) %>% select(GEOID)
miss15income <- fill15income %>% filter(is.na(B19013_001E_income15)) %>% select(GEOID)

get14rent <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25064_001", 
                     year = 2014, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get14house <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25077_001", 
                      year = 2014, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get14income <- get_acs(geography = "tract", state = 51, county = 059, variables = "B19013_001", 
                       year = 2014, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)

fill14rent <- left_join(miss15rent, get14rent, by = "GEOID") %>% rename(B25064_001E_rent14 = B25064_001E) %>% select(GEOID, B25064_001E_rent14) 
fill14house <- left_join(miss15house, get14house, by = "GEOID") %>% rename(B25077_001E_house14 = B25077_001E) %>% select(GEOID, B25077_001E_house14)
fill14income <- left_join(miss15income, get14income, by = "GEOID") %>% rename(B19013_001E_income14 = B19013_001E) %>% select(GEOID, B19013_001E_income14) 

# Try to find data from 09-13 instead
miss14rent <- fill14rent %>% filter(is.na(B25064_001E_rent14)) %>% select(GEOID)
miss14house <- fill14house %>% filter(is.na(B25077_001E_house14)) %>% select(GEOID)
miss14income <- fill14income %>% filter(is.na(B19013_001E_income14)) %>% select(GEOID)

get13rent <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25064_001", 
                     year = 2013, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get13house <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25077_001", 
                      year = 2013, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get13income <- get_acs(geography = "tract", state = 51, county = 059, variables = "B19013_001", 
                       year = 2013, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)

fill13rent <- left_join(miss14rent, get13rent, by = "GEOID") %>% rename(B25064_001E_rent13 = B25064_001E) %>% select(GEOID, B25064_001E_rent13) 
fill13house <- left_join(miss14house, get13house, by = "GEOID") %>% rename(B25077_001E_house13 = B25077_001E) %>% select(GEOID, B25077_001E_house13) 
fill13income <- left_join(miss14income, get13income, by = "GEOID") %>% rename(B19013_001E_income13 = B19013_001E) %>% select(GEOID, B19013_001E_income13)

# Check data from 08-12
miss13rent <- fill13rent %>% filter(is.na(B25064_001E_rent13)) %>% select(GEOID)
miss13house <- fill13house %>% filter(is.na(B25077_001E_house13)) %>% select(GEOID)
miss13income <- fill13income %>% filter(is.na(B19013_001E_income13)) %>% select(GEOID)

get12rent <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25064_001", 
                     year = 2012, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get12house <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25077_001", 
                      year = 2012, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get12income <- get_acs(geography = "tract", state = 51, county = 059, variables = "B19013_001", 
                       year = 2012, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)

fill12rent <- left_join(miss13rent, get12rent, by = "GEOID") %>% rename(B25064_001E_rent12 = B25064_001E) %>% select(GEOID, B25064_001E_rent12) 
fill12house <- left_join(miss13house, get12house, by = "GEOID") %>% rename(B25077_001E_house12 = B25077_001E) %>% select(GEOID, B25077_001E_house12) 
fill12income <- left_join(miss13income, get12income, by = "GEOID") %>% rename(B19013_001E_income12 = B19013_001E) %>% select(GEOID, B19013_001E_income12)

# Put together
fill_rent_front <- left_join(fill17rent, fill16rent)
fill_rent_front <- left_join(fill_rent_front, fill15rent)
fill_rent_front <- left_join(fill_rent_front, fill14rent)
fill_rent_front <- left_join(fill_rent_front, fill13rent)
fill_rent_front <- left_join(fill_rent_front, fill12rent)

fill_house_front <- left_join(fill17house, fill16house)
fill_house_front <- left_join(fill_house_front, fill15house)
fill_house_front <- left_join(fill_house_front, fill14house)
fill_house_front <- left_join(fill_house_front, fill13house)
fill_house_front <- left_join(fill_house_front, fill12house)

fill_income_front <- left_join(fill17income, fill16income)
fill_income_front <- left_join(fill_income_front, fill15income)
fill_income_front <- left_join(fill_income_front, fill14income)
fill_income_front <- left_join(fill_income_front, fill13income)
fill_income_front <- left_join(fill_income_front, fill12income)


#
# MISSING IN 2012 --------------------------------------------------------------------------------------------
#

# Filter to tracts missing data from 08-12
miss12rent <- acs0812 %>% filter(is.na(B25064_001E)) %>% select(GEOID)
miss12house <- acs0812 %>% filter(is.na(B25077_001E)) %>% select(GEOID)
miss12income <- acs0812 %>% filter(is.na(B19013_001E)) %>% select(GEOID)

# Try to find data from 07-11 instead
get11rent <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25064_001", 
                     year = 2011, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get11house <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25077_001", 
                      year = 2011, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get11income <- get_acs(geography = "tract", state = 51, county = 059, variables = "B19013_001", 
                       year = 2011, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)

fill11rent <- left_join(miss12rent, get11rent, by = "GEOID") %>% rename(B25064_001E_rent11 = B25064_001E) %>% select(GEOID, B25064_001E_rent11) %>% st_set_geometry(NULL)
fill11house <- left_join(miss12house, get11house, by = "GEOID") %>% rename(B25077_001E_house11 = B25077_001E) %>% select(GEOID, B25077_001E_house11) %>% st_set_geometry(NULL)
fill11income <- left_join(miss12income, get11income, by = "GEOID") %>% rename(B19013_001E_income11 = B19013_001E) %>% select(GEOID, B19013_001E_income11) %>% st_set_geometry(NULL)

# Try to find data from 08-12 instead
miss11rent <- fill11rent %>% filter(is.na(B25064_001E_rent11)) %>% select(GEOID)
miss11house <- fill11house %>% filter(is.na(B25077_001E_house11)) %>% select(GEOID)
miss11income <- fill11income %>% filter(is.na(B19013_001E_income11)) %>% select(GEOID)

get10rent <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25064_001", 
                     year = 2010, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get10house <- get_acs(geography = "tract", state = 51, county = 059, variables = "B25077_001", 
                      year = 2010, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)
get10income <- get_acs(geography = "tract", state = 51, county = 059, variables = "B19013_001", 
                       year = 2010, survey = "acs5", cache_table = TRUE, output = "wide", geometry = FALSE)

fill10rent <- left_join(miss11rent, get10rent, by = "GEOID") %>% rename(B25064_001E_rent10 = B25064_001E) %>% select(GEOID, B25064_001E_rent10) 
fill10house <- left_join(miss11house, get10house, by = "GEOID") %>% rename(B25077_001E_house10 = B25077_001E) %>% select(GEOID, B25077_001E_house10) 
fill10income <- left_join(miss11income, get10income, by = "GEOID") %>% rename(B19013_001E_income10 = B19013_001E) %>% select(GEOID, B19013_001E_income10)

# Put together
fill_rent_back <- left_join(fill11rent, fill10rent)

fill_house_back <- left_join(fill11house, fill10house)

fill_income_back <- left_join(fill11income, fill10income)


#
# CHECK IF IT'S ALWAYS THE SAME TRACTS --------------------------------------------------------------------------------------------
#

intersect(fill_rent_front$GEOID, fill_rent_back$GEOID)
intersect(fill_house_front$GEOID, fill_house_back$GEOID)
intersect(fill_income_front$GEOID, fill_income_back$GEOID)


#
# FILL IT IN --------------------------------------------------------------------------------------------
#

# Fill in house for 2012
acs0812$backhouse <- NA
acs0812[acs0812$GEOID == "51059421900", ]$backhouse <- 2010
acs0812[acs0812$GEOID == "51059421900", ]$B25077_001E <- 350000

# Fill in rent for 2012
acs0812$backrent <- NA
acs0812[acs0812$GEOID == "51059491101", ]$backrent <- 2011
acs0812[acs0812$GEOID == "51059491101", ]$B25064_001E <- 2001

# Fill in house for 2018
acs1418$fronthouse <- NA
acs1418[acs1418$GEOID == "51059451602", ]$fronthouse <- 2017
acs1418[acs1418$GEOID == "51059451602", ]$B25077_001E <- 233600

# Fill in rent for 2018
acs1418$frontrent <- NA
acs1418[acs1418$GEOID == "51059415600", ]$frontrent <- 2013
acs1418[acs1418$GEOID == "51059415600", ]$B25064_001E <- 2001

acs1418[acs1418$GEOID == "51059481600", ]$frontrent <- 2012
acs1418[acs1418$GEOID == "51059481600", ]$B25064_001E <- 2001

acs1418[acs1418$GEOID == "51059450800", ]$frontrent <- 2017
acs1418[acs1418$GEOID == "51059450800", ]$B25064_001E <- 1462

acs1418[acs1418$GEOID == "51059482400", ]$frontrent <- 2014
acs1418[acs1418$GEOID == "51059482400", ]$B25064_001E <- 2001

acs1418[acs1418$GEOID == "51059480300", ]$frontrent <- 2014
acs1418[acs1418$GEOID == "51059480300", ]$B25064_001E <- 2001

acs1418[acs1418$GEOID == "51059492100", ]$frontrent <- 2015
acs1418[acs1418$GEOID == "51059492100", ]$B25064_001E <- 3501

acs1418[acs1418$GEOID == "51059480201", ]$frontrent <- 2016
acs1418[acs1418$GEOID == "51059480201", ]$B25064_001E <- 3501

acs1418[acs1418$GEOID == "51059480501", ]$frontrent <- 2017
acs1418[acs1418$GEOID == "51059480501", ]$B25064_001E <- 2776

acs1418[acs1418$GEOID == "51059480503", ]$frontrent <- 2016
acs1418[acs1418$GEOID == "51059480503", ]$B25064_001E <- 1935

acs1418[acs1418$GEOID == "51059460100", ]$frontrent <- 2017
acs1418[acs1418$GEOID == "51059460100", ]$B25064_001E <- 3501

acs1418[acs1418$GEOID == "51059491502", ]$frontrent <- 2014
acs1418[acs1418$GEOID == "51059491502", ]$B25064_001E <- 2001