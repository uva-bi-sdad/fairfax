README for Corelogic Data (2-10-2020)

The Corelogic dataset is essentially property data, including information about sales prices, land value, and mortgage lending. As of Feb 2020, SDAD has two types of Corelogic data: (1) Corelogic SDAD Data, which SDAD bought directly from Corelogic, and (2) Corelogic USDA Data, which SDAD can access because of an agreement with USDA for the broadband project (and we can't technically use this for Fairfax). While a considerable amount of this data is overlapping, SDAD bought their data a little bit later than the USDA so it is likely more comprehensive than the USDA version. In the SDAD dataset, we only have tax history data while in the USDA dataset we have both tax and deed data. 

SDAD Corelogic
If you look at the tables using pgAdmin, the Corelogic SDAD Data (i.e. corelogic_sdad.Tables) currently organized into:
* _01 through _09: dump files (contains everything, but note Aaron is having issues with 02 through 09 and they are currently empty)
* _1: "latest tax file"
* _2: "latest tax file" with some property joins (e.g. mortgage data)
_1 and _2 are also split by state FIPS code, e.g. _1_51 is _1 data for VA (state FIPS = 51).

USDA Corelogic
While the usda_deed tables are organized by year (2005-2015), the usda_tax_history data is NOT organized by years. In other words, the numbers (i.e. usda_tax_history_01 - usda_tax_history_10) do NOT correspond to years for the tax history data. Both the tax and deed data should be relatively complete for the years 2005-2015, but there may also be some 2016-2017 data as well, but Neil has suggested that this could be incomplete.