library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(ggthemes)
library(viridis)
library(stringr)
library(hrbrthemes)

# All workforce segments, private jobs only (firm data only available for private jobs), 2011-17 (first time firm data is available).
# Firm size is the NATIONAL SIZE OF THE FIRM in March of the previous year.

#
# Get WAC S000 JT02 2011 ---------------------------------------------------------
#

# Read in
wac_priv_11 <- read_csv("./data/original/lodes/va_wac_S000_JT02_2011.csv", progress = show_progress(), col_names = TRUE, cols(w_geocode = "c", createdate = "c", .default = col_double()))
wac_priv_17 <- read_csv("./data/original/lodes/va_wac_S000_JT02_2017.csv", progress = show_progress(), col_names = TRUE, cols(w_geocode = "c", createdate = "c", .default = col_double()))

# Filter to Fairfax county 
wac_priv_11_ffx <- left_join(xwalk_ffx, wac_priv_11, by = c("tabblk2010" = "w_geocode"))
wac_priv_17_ffx <- left_join(xwalk_ffx, wac_priv_17, by = c("tabblk2010" = "w_geocode"))


#
# WAC S000 JT02 2011 by tract ---------------------------------------------------------
#

# Create variables
wac_priv_11_ffx_trct <- wac_priv_11_ffx %>% arrange(trct) %>%
                                            group_by(trct) %>% 
                                            mutate(job11_all = sum(C000, na.rm = TRUE),
                                                   firmsize11_019 = sum(CFS01, na.rm = TRUE),
                                                   firmsize11_2049 = sum(CFS02, na.rm = TRUE),
                                                   firmsize11_50249 = sum(CFS03, na.rm = TRUE),
                                                   firmsize11_250499 = sum(CFS04, na.rm = TRUE),
                                                   firmsize11_500ovr = sum(CFS05, na.rm = TRUE)) %>%
                                            ungroup()

# Select relevant columns 
wac_priv_11_ffx_trct <- wac_priv_11_ffx_trct %>% select(cty, ctyname, trct, trctname, job11_all, starts_with("firm"))

# Get one row per tract
wac_priv_11_ffx_trct <- wac_priv_11_ffx_trct %>% group_by(trct) %>% slice(1)


#
# WAC S000 JT02 2017 by tract ---------------------------------------------------------
#

# Create variables
wac_priv_17_ffx_trct <- wac_priv_17_ffx %>% arrange(trct) %>%
  group_by(trct) %>% 
  mutate(job17_all = sum(C000, na.rm = TRUE),
         firmsize17_019 = sum(CFS01, na.rm = TRUE),
         firmsize17_2049 = sum(CFS02, na.rm = TRUE),
         firmsize17_50249 = sum(CFS03, na.rm = TRUE),
         firmsize17_250499 = sum(CFS04, na.rm = TRUE),
         firmsize17_500ovr = sum(CFS05, na.rm = TRUE)) %>%
  ungroup()

# Select relevant columns 
wac_priv_17_ffx_trct <- wac_priv_17_ffx_trct %>% select(cty, ctyname, trct, trctname, job17_all, starts_with("firm"))

# Get one row per tract
wac_priv_17_ffx_trct <- wac_priv_17_ffx_trct %>% group_by(trct) %>% slice(1)


#
# Get geography & join---------------------------------------------------------
#

# Get tract geography
ffxcounty <- tracts("VA", county = "059")
ffxcounty <- st_as_sf(ffxcounty)

head(ffxcounty)

# Join 2009 and 2017 data
wac_firm_1117 <- left_join(wac_priv_11_ffx_trct, wac_priv_17_ffx_trct, by = c("cty", "ctyname", "trct", "trctname"))

# Join data with geography
wac_firm_1117geo <- left_join(ffxcounty, wac_firm_1117, by = c("GEOID" = "trct"))

# Check CRS
st_crs(wac_firm_1117geo)
wac_firm_1117geo <- wac_firm_1117geo %>% st_transform("+proj=longlat +datum=WGS84")


#
# Create growth variables ---------------------------------------------------------
#

# Continuous
wac_firm_1117geo <- wac_firm_1117geo %>% 
  mutate(growsize_019 = ((firmsize17_019*100)/firmsize11_019) - 100,
         growsize_2049 = ((firmsize17_2049*100)/firmsize11_2049) - 100,
         growsize_50249 = ((firmsize17_50249*100)/firmsize11_50249) - 100,
         growsize_250499 = ((firmsize17_250499*100)/firmsize11_250499) - 100,
         growsize_500ovr = ((firmsize17_500ovr*100)/firmsize11_500ovr) - 100)

# Categorical
wac_firm_1117geo <- wac_firm_1117geo %>% 
  mutate(growsize_019 = ifelse((is.na(growsize_019)| growsize_019 == Inf | is.nan(growsize_019)), NA, growsize_019),
         growsize_2049 = ifelse((is.na(growsize_2049) | growsize_2049 == Inf | is.nan(growsize_2049)), NA, growsize_2049),
         growsize_50249 = ifelse((is.na(growsize_50249) | growsize_50249 == Inf | is.nan(growsize_50249)), NA, growsize_50249),
         growsize_250499 = ifelse((is.na(growsize_250499) | growsize_250499 == Inf | is.nan(growsize_250499)), NA, growsize_250499),
         growsize_500ovr = ifelse((is.na(growsize_500ovr) | growsize_500ovr == Inf | is.nan(growsize_500ovr)), NA, growsize_500ovr))  %>% 
  mutate(cat_growsize_019 = case_when(growsize_019 <= -50 ~ "<= -50%",
                                      growsize_019 > -50 & growsize_019 <= 0 ~ "-49-0%",
                                      growsize_019 > 0 & growsize_019 <= 50  ~ "1-50%",
                                      growsize_019 > 50 & growsize_019 <= 100  ~ "51-100%",
                                      growsize_019 > 100 & growsize_019 <= 150  ~ "101-150%",
                                      growsize_019 > 150 ~ ">= 150%",
                                      is.na(growsize_019) ~ NA_character_),
         cat_growsize_2049 = case_when(growsize_2049 <= -50 ~ "<= -50%",
                                       growsize_2049 > -50 & growsize_2049 <= 0 ~ "-49-0%",
                                       growsize_2049 > 0 & growsize_2049 <= 50  ~ "1-50%",
                                       growsize_2049 > 50 & growsize_2049 <= 100  ~ "51-100%",
                                       growsize_2049 > 100 & growsize_2049 <= 150  ~ "101-150%",
                                       growsize_2049 > 150 ~ ">= 150%",
                                       is.na(growsize_2049) ~ NA_character_),
         cat_growsize_50249 = case_when(growsize_50249 <= -50 ~ "<= -50%",
                                        growsize_50249 > -50 & growsize_50249 <= 0 ~ "-49-0%",
                                        growsize_50249 > 0 & growsize_50249 <= 50  ~ "1-50%",
                                        growsize_50249 > 50 & growsize_50249 <= 100  ~ "51-100%",
                                        growsize_50249 > 100 & growsize_50249 <= 150  ~ "101-150%",
                                        growsize_50249 > 150 ~ ">= 150%",
                                        is.na(growsize_50249) ~ NA_character_),
         cat_growsize_250499 = case_when(growsize_250499 <= -50 ~ "<= -50%",
                                         growsize_250499 > -50 & growsize_250499 <= 0 ~ "-49-0%",
                                         growsize_250499 > 0 & growsize_250499 <= 50  ~ "1-50%",
                                         growsize_250499 > 50 & growsize_250499 <= 100  ~ "51-100%",
                                         growsize_250499 > 100 & growsize_250499 <= 150  ~ "101-150%",
                                         growsize_250499 > 150 ~ ">= 150%",
                                         is.na(growsize_250499) ~ NA_character_),
         cat_growsize_500ovr = case_when(growsize_500ovr <= -50 ~ "<= -50%",
                                         growsize_500ovr > -50 & growsize_500ovr <= 0 ~ "-49-0%",
                                         growsize_500ovr > 0 & growsize_500ovr <= 50  ~ "1-50%",
                                         growsize_500ovr > 50 & growsize_500ovr <= 100  ~ "51-100%",
                                         growsize_500ovr > 100 & growsize_500ovr <= 150  ~ "101-150%",
                                         growsize_500ovr > 150 ~ ">= 150%",
                                         is.na(growsize_500ovr) ~ NA_character_))

wac_firm_1117geo$cat_growsize_019 <- factor(wac_firm_1117geo$cat_growsize_019, levels = c("<= -50%", "-49-0%", "1-50%", "51-100%", "101-150%", ">= 150%"))
wac_firm_1117geo$cat_growsize_2049 <- factor(wac_firm_1117geo$cat_growsize_2049, levels = c("<= -50%", "-49-0%", "1-50%", "51-100%", "101-150%", ">= 150%"))
wac_firm_1117geo$cat_growsize_50249 <- factor(wac_firm_1117geo$cat_growsize_50249, levels = c("<= -50%", "-49-0%", "1-50%", "51-100%", "101-150%", ">= 150%"))
wac_firm_1117geo$cat_growsize_250499 <- factor(wac_firm_1117geo$cat_growsize_250499, levels = c("<= -50%", "-49-0%", "1-50%", "51-100%", "101-150%", ">= 150%"))
wac_firm_1117geo$cat_growsize_500ovr <- factor(wac_firm_1117geo$cat_growsize_500ovr, levels = c("<= -50%", "-49-0%", "1-50%", "51-100%", "101-150%", ">= 150%"))

                                                                                                     
#
# Plot ----------------------------------------------------------------------------------------
#

test <- wac_firm_1117geo %>% select(trctname, growsize_019, cat_growsize_019, firmsize11_019, firmsize17_019)

# Number of jobs for workers at firms size 0-19 employees
ggplot(data = wac_firm_1117geo) +
  geom_sf(aes(fill = cat_growsize_019), size = 0.001) +
  labs(title = "Percent change in number of jobs at firms size 0-19 employees", 
       subtitle = "Data shown for change in private jobs only, from 2011 to 2017 in Fairfax County by tract.\nFirm size refers to national size.\nNA = No such jobs reported in 2011; percent change is uninformative.",
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Percent change", na.value = "#e9e9e9", values = c("<= -50%" = "#D73027",
                                                                              "-49-0%" = "#FC8D59",
                                                                              "1-50%" = "#D9EF8B",
                                                                              "51-100%" = "#A6D96A",
                                                                              "101-150%" = "#66BD63",
                                                                              ">= 150%" = "#1A9850"))
ggsave("./docs/lodes/chg_firmsize0-19.png")

# Number of jobs for workers at firms size 20-49 employees
ggplot(data = wac_firm_1117geo) +
  geom_sf(aes(fill = cat_growsize_2049), size = 0.001) +
  labs(title = "Percent change in number of jobs at firms size 20-49 employees", 
       subtitle = "Data shown for change in private jobs only, from 2011 to 2017 in Fairfax County by tract.\nFirm size refers to national size.\nNA = No such jobs reported in 2011; percent change is uninformative.",
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Percent change", na.value = "#e9e9e9", values = c("<= -50%" = "#D73027",
                                                                              "-49-0%" = "#FC8D59",
                                                                              "1-50%" = "#D9EF8B",
                                                                              "51-100%" = "#A6D96A",
                                                                              "101-150%" = "#66BD63",
                                                                              ">= 150%" = "#1A9850"))
ggsave("./docs/lodes/chg_firmsize20-49.png")

# Number of jobs for workers at firms size 50-249 employees
ggplot(data = wac_firm_1117geo) +
  geom_sf(aes(fill = cat_growsize_50249), size = 0.001) +
  labs(title = "Percent change in number of jobs at firms size 50-249 employees", 
       subtitle = "Data shown for change in private jobs only, from 2011 to 2017 in Fairfax County by tract.\nFirm size refers to national size.\nNA = No such jobs reported in 2011; percent change is uninformative.",
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Percent change", na.value = "#e9e9e9", values = c("<= -50%" = "#D73027",
                                                                              "-49-0%" = "#FC8D59",
                                                                              "1-50%" = "#D9EF8B",
                                                                              "51-100%" = "#A6D96A",
                                                                              "101-150%" = "#66BD63",
                                                                              ">= 150%" = "#1A9850"))
ggsave("./docs/lodes/chg_firmsize50-249.png")

# Number of jobs for workers at firms size 250-499 employees
ggplot(data = wac_firm_1117geo) +
  geom_sf(aes(fill = cat_growsize_250499), size = 0.001) +
  labs(title = "Percent change in number of jobs at firms size 250-499 employees", 
       subtitle = "Data shown for change in private jobs only, from 2011 to 2017 in Fairfax County by tract.\nFirm size refers to national size.\nNA = No such jobs reported in 2011; percent change is uninformative.",
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Percent change", na.value = "#e9e9e9", values = c("<= -50%" = "#D73027",
                                                                              "-49-0%" = "#FC8D59",
                                                                              "1-50%" = "#D9EF8B",
                                                                              "51-100%" = "#A6D96A",
                                                                              "101-150%" = "#66BD63",
                                                                              ">= 150%" = "#1A9850"))
ggsave("./docs/lodes/chg_firmsize250-499.png")

# Number of jobs for workers at firms size 500+ employees
ggplot(data = wac_firm_1117geo) +
  geom_sf(aes(fill = cat_growsize_500ovr), size = 0.001) +
  labs(title = "Percent change in number of jobs at firms size 500+ employees", 
       subtitle = "Data shown for change in private jobs only, from 2011 to 2017 in Fairfax County by tract.\nFirm size refers to national size.\nNA = No such jobs reported in 2011; percent change is uninformative.",
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_manual(name = "Percent change", na.value = "#e9e9e9", values = c("<= -50%" = "#D73027",
                                                                              "-49-0%" = "#FC8D59",
                                                                              "1-50%" = "#D9EF8B",
                                                                              "51-100%" = "#A6D96A",
                                                                              "101-150%" = "#66BD63",
                                                                              ">= 150%" = "#1A9850"))
ggsave("./docs/lodes/chg_firmsize500ovr.png")


#
# Calculate variables at county level (aggregate from tract): 2011 ------------------------------
#

# Calculate variables
wac_priv_11_ffx_cty <- wac_priv_11_ffx %>% group_by(cty) %>%
  mutate(job11_all = sum(C000, na.rm = TRUE),
         firmsize11_019 = sum(CFS01, na.rm = TRUE),
         firmsize11_2049 = sum(CFS02, na.rm = TRUE),
         firmsize11_50249 = sum(CFS03, na.rm = TRUE),
         firmsize11_250499 = sum(CFS04, na.rm = TRUE),
         firmsize11_500ovr = sum(CFS05, na.rm = TRUE)) %>%
  ungroup()

# Select relevant columns 
ffxcounty_firm11 <- wac_priv_11_ffx_cty %>% select(cty, ctyname, job11_all, starts_with("firm"))

# Get one row per county
ffxcounty_firm11 <- ffxcounty_firm11 %>% group_by(cty) %>% slice(1)


#
# Calculate variables at county level (aggregate from tract): 2017 ------------------------------
#

# Calculate variables
wac_priv_17_ffx_cty <- wac_priv_17_ffx %>% group_by(cty) %>%
  mutate(job17_all = sum(C000, na.rm = TRUE),
         firmsize17_019 = sum(CFS01, na.rm = TRUE),
         firmsize17_2049 = sum(CFS02, na.rm = TRUE),
         firmsize17_50249 = sum(CFS03, na.rm = TRUE),
         firmsize17_250499 = sum(CFS04, na.rm = TRUE),
         firmsize17_500ovr = sum(CFS05, na.rm = TRUE)) %>%
  ungroup()

# Select relevant columns 
ffxcounty_firm17 <- wac_priv_17_ffx_cty %>% select(cty, ctyname, job17_all, starts_with("firm"))

# Get one row per county
ffxcounty_firm17 <- ffxcounty_firm17 %>% group_by(cty) %>% slice(1)


#
# Merge & pivot & clean ------------------------------------------------------------------------------------------
#

# Pivot
ffxcounty_firm11_long <- ffxcounty_firm11 %>% pivot_longer(cols = 3:8, values_to = "val11")
ffxcounty_firm17_long <- ffxcounty_firm17 %>% pivot_longer(cols = 3:8, values_to = "val17")

# Merge
ffxcounty_firm11_long$name <- str_replace_all(ffxcounty_firm11_long$name, "firmsize11_", "firmsize11_size")
ffxcounty_firm11_long$name <- str_remove(ffxcounty_firm11_long$name, "firmsize11_")
ffxcounty_firm11_long$name <- str_remove(ffxcounty_firm11_long$name, "job11_")
ffxcounty_firm17_long$name <- str_replace_all(ffxcounty_firm17_long$name, "firmsize17_", "firmsize17_size")
ffxcounty_firm17_long$name <- str_remove(ffxcounty_firm17_long$name, "firmsize17_")
ffxcounty_firm17_long$name <- str_remove(ffxcounty_firm17_long$name, "job17_")
ctydata <- left_join(ffxcounty_firm11_long, ffxcounty_firm17_long)

# Create growth variable
ctydata <- ctydata %>% mutate(growth = ((val17*100)/val11) - 100)

# Clean
ctydata$name <- c("All private jobs", "Firm size 0-19", "Firm size 20-49", "Firm size 50-249", "Firm size 250-499", "Firm size 500+")
ctydata$name <- factor(ctydata$name, levels = c("Firm size 0-19", "Firm size 20-49", "Firm size 50-249", "Firm size 250-499", "Firm size 500+", "All private jobs"))


#
# Plot - lollipop ------------------------------------------------------------------------------------------
#

# Plot
ggplot(ctydata) +
  geom_segment(aes(x = name, xend = name, y = 0, yend = growth), color = "darkgrey") +
  geom_point(aes(x = name, y = growth, color = rgb(0.7, 0.2, 0.1, 0.5)), size = 4) +
  coord_flip() +
  theme_ipsum() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12), limits = c(0, 12)) +
  labs(x = "Firm size (# employees)", y = "Growth in number of jobs (%)", caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.",
       title = "Percent growth in the number of jobs held by individuals at firms by size", subtitle = "Data shown for change between 20011 and 2017 for private jobs in Fairfax County.\nFirm size refers to national size.") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "none") +
  scale_color_manual(name = "Year", values = rgb(0.7, 0.2, 0.1, 0.5))

# Save
ggsave("./docs/lodes/firmsize_lollipop.png", plot = last_plot(), device = "png")
