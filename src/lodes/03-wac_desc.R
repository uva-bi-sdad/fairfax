library(dplyr)
library(ggplot2)
library(ggthemes)



# Note: 01-read to read in data, 02-wac_create to create the dataset


#
# Calculate change variables ---------------------------------------------------------------------------
#

change <- wac0917geo %>% mutate(chg_female = job17_female - job09_female,
                             chg_male = job17_male - job09_male,
                             chg_age29less = job17_age29less - job09_age29less,
                             chg_age3054 = job17_age3054 - job09_age3054,
                             chg_age55ovr = job17_age55ovr - job09_age55ovr,
                             chg_earn1250less = job17_earn1250less - job09_earn1250less,
                             chg_earn12513333 = job17_earn12513333 - job09_earn12513333,
                             chg_earn3334ovr = job17_earn3334ovr - job09_earn3334ovr,
                             chg_white = job17_white - job09_white,
                             chg_black = job17_black - job09_black,
                             chg_native = job17_native - job09_native,
                             chg_asian = job17_asian - job09_asian,
                             chg_pacific = job17_pacific - job09_pacific,
                             chg_multirace = job17_multirace - job09_multirace,
                             chg_hisp = job17_hisp - job09_hisp,
                             chg_nonhisp = job17_nonhisp - job09_nonhisp,
                             chg_educlesshs = job17_educlesshs - job09_educlesshs,
                             chg_educhsequiv = job17_educhsequiv - job09_educhsequiv,
                             chg_educsomecol = job17_educsomecol - job09_educsomecol,
                             chg_educba = job17_educba - job09_educba)


#
# Plot ----------------------------------------------------------------------------------------
#

# Female
ggplot(data = change) +
  geom_sf(aes(fill = chg_female), size = 0.001) +
  labs(title = "Proportion change in female-held jobs", 
       subtitle = "Data shown for change from 2009 to 2017 in Fairfax County by tract.",
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Proportion change", limits = c(-0.3, 0.3), breaks = c(-0.3, -0.15, 0, 0.15, 0.3))
ggsave("./docs/lodes/chg_female.png")

# Hispanic
ggplot(data = change) +
  geom_sf(aes(fill = chg_hisp), size = 0.001) +
  labs(title = "Proportion change in Hispanic-held jobs in Fairfax County", 
       subtitle = "Data shown for change from 2009 to 2017 in Fairfax County by tract.",
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Proportion change", limits = c(-0.3, 0.3), breaks = c(-0.3, -0.15, 0, 0.15, 0.3))
ggsave("./docs/lodes/chg_hispanic.png")

# Black
ggplot(data = change) +
  geom_sf(aes(fill = chg_black), size = 0.001) +
  labs(title = "Proportion change in Black-held jobs in Fairfax County", 
       subtitle = "Data shown for change from 2009 to 2017 in Fairfax County by tract.",
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Proportion change", limits = c(-0.3, 0.3), breaks = c(-0.3, -0.15, 0, 0.15, 0.3))
ggsave("./docs/lodes/chg_black.png")

# $3,334
ggplot(data = change) +
  geom_sf(aes(fill = chg_earn3334ovr), size = 0.001) +
  labs(title = "Proportion change in jobs netting >$3,333 per month", 
       subtitle = "Data shown for change from 2009 to 2017 in Fairfax County by tract.", 
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Proportion change", limits = c(-0.3, 0.3), breaks = c(-0.3, -0.15, 0, 0.15, 0.3))
ggsave("./docs/lodes/chg_earn3333.png")

# $1,251 - 3,333
ggplot(data = change) +
  geom_sf(aes(fill = chg_earn12513333), size = 0.001) +
  labs(title = "Proportion change in jobs netting $1,251 to 3,333 per month", 
       subtitle = "Data shown for change from 2009 to 2017 in Fairfax County by tract.", 
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Proportion change", limits = c(-0.3, 0.3), breaks = c(-0.3, -0.15, 0, 0.15, 0.3))
ggsave("./docs/lodes/chg_earn12513333.png")

# <$1,250
ggplot(data = change) +
  geom_sf(aes(fill = chg_earn1250less), size = 0.001) +
  labs(title = "Proportion change in jobs netting <$1,250 per month", 
       subtitle = "Data shown for change from 2009 to 2017 in Fairfax County by tract.", 
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Proportion change", limits = c(-0.3, 0.3), breaks = c(-0.3, -0.15, 0, 0.15, 0.3))
ggsave("./docs/lodes/chg_earn1250.png")

# <HS
ggplot(data = change) +
  geom_sf(aes(fill = chg_educlesshs), size = 0.001) +
  labs(title = "Proportion change in jobs held by persons with less than high school education", 
       subtitle = "Data shown for change from 2009 to 2017 in Fairfax County by tract.", 
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Proportion change", limits = c(-0.3, 0.3), breaks = c(-0.3, -0.15, 0, 0.15, 0.3))
ggsave("./docs/lodes/chg_educlesshs.png")

# HS
ggplot(data = change) +
  geom_sf(aes(fill = chg_educhsequiv), size = 0.001) +
  labs(title = "Proportion change in jobs held by persons with high school education", 
       subtitle = "Data shown for change from 2009 to 2017 in Fairfax County by tract.", 
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Proportion change", limits = c(-0.3, 0.3), breaks = c(-0.3, -0.15, 0, 0.15, 0.3))
ggsave("./docs/lodes/chg_educhs.png")

# Some college
ggplot(data = change) +
  geom_sf(aes(fill = chg_educsomecol), size = 0.001) +
  labs(title = "Proportion change in jobs held by persons with some college education", 
       subtitle = "Data shown for change from 2009 to 2017 in Fairfax County by tract.", 
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Proportion change", limits = c(-0.3, 0.3), breaks = c(-0.3, -0.15, 0, 0.15, 0.3))
ggsave("./docs/lodes/chg_educsomecoll.png")

# BA+
ggplot(data = change) +
  geom_sf(aes(fill = chg_educba), size = 0.001) +
  labs(title = "Proportion change in jobs held by persons with BA or higher education", 
       subtitle = "Data shown for change from 2009 to 2017 in Fairfax County by tract.", 
       caption = "Source: LEHD Origin-Destination Employment Statistics (LODES) data.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Proportion change", limits = c(-0.3, 0.3), breaks = c(-0.3, -0.15, 0, 0.15, 0.3))
ggsave("./docs/lodes/chg_educba.png")