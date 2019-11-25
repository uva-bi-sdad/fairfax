library(dplyr)
library(ggplot2)
library(forcats)
library(ggthemes)
library(viridis)
library(tidyr)
library(stringr)
library(hrbrthemes)


# Note: 01-read to read in data, 02-wac_create to get the dataset
options(scipen = 999)

#
# Calculate variables at county level (aggregate from tract): 2017 ------------------------------
#

# Calculate variables
ffxcounty17 <- wac17_ffx %>% group_by(cty) %>%
                         mutate(job17_all = sum(C000, na.rm = TRUE),
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
ffxcounty17 <- ffxcounty17 %>% select(cty, ctyname, starts_with("job17_"))

# Get one row per county
ffxcounty17 <- ffxcounty17 %>% group_by(cty) %>% slice(1)


#
# Calculate variables at county level (aggregate from tract): 2009 ------------------------------
#

# Calculate variables
ffxcounty09 <- wac09_ffx %>% group_by(cty) %>%
                            mutate(job09_all = sum(C000, na.rm = TRUE),
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
ffxcounty09 <- ffxcounty09 %>% select(cty, ctyname, starts_with("job09_"))

# Get one row per county
ffxcounty09 <- ffxcounty09 %>% group_by(cty) %>% slice(1)


#
# Merge & pivot & clean ------------------------------------------------------------------------------------------
#

# Pivot
ffxcounty09_long <- ffxcounty09 %>% pivot_longer(cols = 3:23, values_to = "val09")
ffxcounty17_long <- ffxcounty17 %>% pivot_longer(cols = 3:23, values_to = "val17")

# Merge
ffxcounty09_long$name <- str_remove(ffxcounty09_long$name, "job09_")
ffxcounty17_long$name <- str_remove(ffxcounty17_long$name, "job17_")
ctydata <- left_join(ffxcounty09_long, ffxcounty17_long)

# Clean
ctydata <- ctydata %>% mutate(meanpoint = (val09 + val17) / 2)
ctydata$name <- c("All jobs", "Age <=29", "Age 30-54", "Age >=55", "Earn <=$1,250", "Earn $1,251-$3,333", "Earn >=$3,334",
                  "White", "Black", "Native American", "Asian", "Hawaii/Pacific Islander", "Multiracial", "Hispanic", "Non Hispanic", 
                  "Education <HS", "Education HS or equivalent", "Education some college or associate's", "Education BA or more", "Male", "Female")

ctydata$name <- factor(ctydata$name, levels = c("All jobs", "Age <=29", "Age 30-54", "Age >=55", "Earn <=$1,250", "Earn $1,251-$3,333", "Earn >=$3,334",
                                                "White", "Black", "Native American", "Asian", "Hawaii/Pacific Islander", "Multiracial", "Hispanic", "Non Hispanic", 
                                                "Education <HS", "Education HS or equivalent", "Education some college or associate's", "Education BA or more", "Male", "Female"))

# Remove all jobs row
ctydata <- ctydata[-1, ]

#
# Plot - lollipop ------------------------------------------------------------------------------------------
#

# Plot
ggplot(ctydata) +
  geom_segment(aes(x = name, xend = name, y = val09, yend = val17), color = "grey") +
  geom_point(aes(x = name, y = val09, color = rgb(0.2, 0.7, 0.1, 0.5)), size = 3) +
  geom_point(aes(x = name, y = val17, color = rgb(0.7, 0.2, 0.1, 0.5)), size = 3) +
  coord_flip() +
  theme_ipsum() +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), limits = c(0, 1)) +
  labs(x = "Variable", y = "Proportion of jobs within category", caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.",
       title = "Change in proportion of jobs held by individual socioeconomic characteristics", subtitle = "Data shown for change between 2009 and 2017 in Fairfax County.") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "bottom") +
  scale_color_manual(name = "Year", 
                     labels = c(2009, 2017),
                     values = c(rgb(0.2, 0.7, 0.1, 0.5), rgb(0.7, 0.2, 0.1, 0.5)))
                     
# Save
ggsave("./docs/lodes/lollipop.png", plot = last_plot(), device = "png")