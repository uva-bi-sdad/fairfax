library(tidyr)
library(ggplot2)

options(scipen = 999)

descriptives <- data

# In a different universe I loop over these.

#
# Descriptive plots --------------------------------------------------------------------------
#

# Median household income
minhhinc <- floor(min(c(min(descriptives$tct_hhinc18), min(descriptives$tct_hhinc12))))
maxhhinc <- ceiling(max(c(max(descriptives$tct_hhinc18), max(descriptives$tct_hhinc12))))

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_hhinc18)) +
  labs(title = "Median household income, 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Median\nhousehold\nincome",
                       limits = c(minhhinc, maxhhinc), 
                       breaks = seq(minhhinc, maxhhinc, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_hhinc18.png", plot = last_plot(), height = 7, width = 7)

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_hhinc12)) +
  labs(title = "Median household income, 2008/12") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis_c(name = "Median\nhousehold\nincome",
                       limits = c(minhhinc, maxhhinc), 
                       breaks = seq(minhhinc, maxhhinc, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_hhinc12.png", plot = last_plot(), height = 7, width = 7)

# No BA degree
minnoba <- floor(min(c(min(descriptives$tct_noba18), min(descriptives$tct_noba12))))
maxnoba <- ceiling(max(c(max(descriptives$tct_noba18), max(descriptives$tct_noba12))))

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_noba18)) +
  labs(title = "% without BA, 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "% without\nBA",
                     limits = c(minnoba, maxnoba), 
                     breaks = seq(minnoba, maxnoba, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_noba18.png", plot = last_plot(), height = 7, width = 7)

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_noba12)) +
  labs(title = "% without BA, 2008/12") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "% without\nBA",
                     limits = c(minnoba, maxnoba), 
                     breaks = seq(minnoba, maxnoba, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_noba12.png", plot = last_plot(), height = 7, width = 7)

# Nonwhite
minnonwhite <- floor(min(c(min(descriptives$tct_nonwhite18), min(descriptives$tct_nonwhite12))))
maxnonwhite <- ceiling(max(c(max(descriptives$tct_nonwhite18), max(descriptives$tct_nonwhite12))))

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_nonwhite18)) +
  labs(title = "% non-white, 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "% non-white",
                     limits = c(minnonwhite, maxnonwhite), 
                     breaks = seq(minnonwhite, maxnonwhite, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_nonwhite18.png", plot = last_plot(), height = 7, width = 7)

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_nonwhite12)) +
  labs(title = "% non-white, 2008/12") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "% non-white",
                     limits = c(minnonwhite, maxnonwhite), 
                     breaks = seq(minnonwhite, maxnonwhite, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_nonwhite12.png", plot = last_plot(), height = 7, width = 7)

# Renters
minrenters <- floor(min(c(min(descriptives$tct_renters18), min(descriptives$tct_renters12))))
maxrenters <- ceiling(max(c(max(descriptives$tct_renters18), max(descriptives$tct_renters12))))

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_renters18)) +
  labs(title = "% renters, 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "% renters",
                     limits = c(minrenters, maxrenters), 
                     breaks = seq(minrenters, maxrenters, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_renters18.png", plot = last_plot(), height = 7, width = 7)

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_renters12)) +
  labs(title = "% renters, 2008/12") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "% renters",
                     limits = c(minrenters, maxrenters), 
                     breaks = seq(minrenters, maxrenters, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_renters12.png", plot = last_plot(), height = 7, width = 7)

# Non Hispanic white
minnonhispwh <- floor(min(c(min(descriptives$tct_nonhispwh18), min(descriptives$tct_nonhispwh12))))
maxnonhispwh <- ceiling(max(c(max(descriptives$tct_nonhispwh18), max(descriptives$tct_nonhispwh12))))

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_nonhispwh18)) +
  labs(title = "% non-Hispanic white, 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "% non-Hispanic\nwhite",
                     limits = c(minnonhispwh, maxnonhispwh), 
                     breaks = seq(minnonhispwh, maxnonhispwh, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_nonhispwh18.png", plot = last_plot(), height = 7, width = 7)

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_nonhispwh12)) +
  labs(title = "% non-Hispanic white, 2008/12") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "% non-Hispanic\nwhite",
                     limits = c(minnonhispwh, maxnonhispwh), 
                     breaks = seq(minnonhispwh, maxnonhispwh, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_nonhispwh12.png", plot = last_plot(), height = 7, width = 7)

# Median gross rent
minmedrent <- floor(min(c(min(descriptives$tct_medrent18), min(descriptives$tct_medrent12))))
maxmedrent <- ceiling(max(c(max(descriptives$tct_medrent18), max(descriptives$tct_medrent12))))

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_medrent18)) +
  labs(title = "Median gross rent, 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "Median gross\nrent",
                     limits = c(minmedrent, maxmedrent), 
                     breaks = seq(minmedrent, maxmedrent, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_medrent18.png", plot = last_plot(), height = 7, width = 7)

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_medrent12)) +
  labs(title = "Median gross rent, 2008/12") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "Median gross\nrent",
                     limits = c(minmedrent, maxmedrent), 
                     breaks = seq(minmedrent, maxmedrent, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_medrent12.png", plot = last_plot(), height = 7, width = 7)

# House value
minmedhome <- floor(min(c(min(descriptives$tct_medhome18), min(descriptives$tct_medhome12))))
maxmedhome <- ceiling(max(c(max(descriptives$tct_medhome18), max(descriptives$tct_medhome12))))

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_medhome18)) +
  labs(title = "Median house value, 2014/18") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "Median house\nvalue",
                     limits = c(minmedhome, maxmedhome), 
                     breaks = seq(minmedhome, maxmedhome, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_medhome18.png", plot = last_plot(), height = 7, width = 7)

ggplot() +
  geom_sf(data = ffxgeo, size = 0.2, fill = "#F0F0F0") +
  geom_sf(data = descriptives, size = 0.2, aes(fill = tct_medhome12)) +
  labs(title = "Median house value, 2008/12") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11)) +
  scale_fill_viridis(name = "Median house\nvalue",
                     limits = c(minmedhome, maxmedhome), 
                     breaks = seq(minmedhome, maxmedhome, length.out = 5))
ggsave(path = "./docs/gentri/", device = "png", filename = "tct_medhome12.png", plot = last_plot(), height = 7, width = 7)


#
# Descriptive tables ----------------------------------------------------------------
#

out <- descriptives %>% st_set_geometry(NULL) %>%
                  group_by(type1218) %>% 
                  summarize(count = n(),
                            mean_hhinc12 = mean(tct_hhinc12),
                            min_hinc12 = min(tct_hhinc12),
                            max_hhinc12 = max(tct_hhinc12),
                            sd_hhinc12 = sd(tct_hhinc12),
                            mean_hhinc18 = mean(tct_hhinc18),
                            min_hhinc18 = min(tct_hhinc18),
                            max_hhinc18 = max(tct_hhinc18),
                            sd_hhinc18 = sd(tct_hhinc18),
                            mean_noba12 = mean(tct_noba12),
                            min_noba12 = min(tct_noba12),
                            max_noba12 = max(tct_noba12),
                            sd_noba12 = sd(tct_noba12),
                            mean_noba18 = mean(tct_noba18),
                            min_noba18 = min(tct_noba18),
                            max_noba18 = max(tct_noba18),
                            sd_noba18 = sd(tct_noba18),
                            mean_nonwhite12 = mean(tct_nonwhite12),
                            min_nonwhite12 = min(tct_nonwhite12),
                            max_nonwhite12 = max(tct_nonwhite12),
                            sd_nonwhite12 = sd(tct_nonwhite12),
                            mean_nonwhite18 = mean(tct_nonwhite18),
                            min_nonwhite18 = min(tct_nonwhite18),
                            max_nonwhite18 = max(tct_nonwhite18),
                            sd_nonwhite18 = sd(tct_nonwhite18),
                            mean_renters12 = mean(tct_renters12),
                            min_renters12 = min(tct_renters12),
                            max_renters12 = max(tct_renters12),
                            sd_renters12 = sd(tct_renters12),
                            mean_renters18 = mean(tct_renters18),
                            min_renters18 = min(tct_renters18),
                            max_renters18 = max(tct_renters18),
                            sd_renters18 = sd(tct_renters18),
                            mean_withba12 = mean(tct_withba12),
                            min_withba12 = min(tct_withba12),
                            max_withba12 = max(tct_withba12),
                            sd_withba12 = sd(tct_withba12),
                            mean_withba18 = mean(tct_withba18),
                            min_withba18 = min(tct_withba18),
                            max_withba18 = max(tct_withba18),
                            sd_withba18 = sd(tct_withba18),
                            mean_medinc12 = mean(tct_medinc12),
                            min_medinc12 = min(tct_medinc12),
                            max_medinc12 = max(tct_medinc12),
                            sd_medinc12 = sd(tct_medinc12),
                            mean_medinc18 = mean(tct_medinc18),
                            min_medinc18 = min(tct_medinc18),
                            max_medinc18 = max(tct_medinc18),
                            sd_medinc18 = sd(tct_medinc18),
                            mean_nonhispwh12 = mean(tct_nonhispwh12),
                            min_nonhispwh12 = min(tct_nonhispwh12),
                            max_nonhispwh12 = max(tct_nonhispwh12),
                            sd_nonhispwh12 = sd(tct_nonhispwh12),
                            mean_nonhispwh18 = mean(tct_nonhispwh18),
                            min_nonhispwh18 = min(tct_nonhispwh18),
                            max_nonhispwh18 = max(tct_nonhispwh18),
                            sd_nonhispwh18 = sd(tct_nonhispwh18),
                            mean_medrent12 = mean(tct_medrent12),
                            min_medrent12 = min(tct_medrent12),
                            max_medrent12 = max(tct_medrent12),
                            sd_medrent12 = sd(tct_medrent12),
                            mean_medrent18 = mean(tct_medrent18),
                            min_medrent18 = min(tct_medrent18),
                            max_medrent18 = max(tct_medrent18),
                            sd_medrent18 = sd(tct_medrent18),
                            mean_medhome12 = mean(tct_medhome12),
                            min_medhome12 = min(tct_medhome12),
                            max_medhome12 = max(tct_medhome12),
                            sd_medhome12 = sd(tct_medhome12),
                            mean_medhome18 = mean(tct_medhome18),
                            min_medhome18 = min(tct_medhome18),
                            max_medhome18 = max(tct_medhome18),
                            sd_medhome18 = sd(tct_medhome18),
                            mean_chgba = mean(chgba),
                            min_chgba = min(chgba),
                            max_chgba = max(chgba),
                            sd_chgba = sd(chgba),
                            mean_chghhinc = mean(chghhinc),
                            min_chghhinc = min(chghhinc),
                            max_chghhinc = max(chghhinc),
                            sd_chghhinc = sd(chghhinc),
                            mean_chgnonhispwh = mean(chgnonhispwh),
                            min_chgnonhispwh = min(chgnonhispwh),
                            max_chgnonhispwh = max(chgnonhispwh),
                            sd_chgnonhispwh = sd(chgnonhispwh),
                            mean_chgmedrent = mean(chgmedrent),
                            min_chgmedrent = min(chgmedrent),
                            max_chgmedrent = max(chgmedrent),
                            sd_chgmedrent = sd(chgmedrent),
                            mean_chgmedhome = mean(chgmedhome),
                            min_chgmedhome = min(chgmedhome),
                            max_chgmedhome = max(chgmedhome),
                            sd_chgmedhome = sd(chgmedhome)
                            ) %>% ungroup()

out <- out %>% pivot_longer(cols = 2:94)

write.csv(out, "./docs/gentri/descriptives.csv")