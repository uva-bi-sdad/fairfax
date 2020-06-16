library(dplyr)
library(tidyr)

out <- rundata %>% st_set_geometry(NULL) %>%
  group_by(type1218) %>% 
  summarize(count = n(),
            mean_chg1218_tct_multunit = mean(chg1218_tct_multunit),
            min_chg1218_tct_multunit = min(chg1218_tct_multunit),
            max_chg1218_tct_multunit = max(chg1218_tct_multunit),
            sd_chg1218_tct_multunit = sd(chg1218_tct_multunit),
            
            mean_chg1218_tct_vacant = mean(chg1218_tct_vacant),
            min_chg1218_tct_vacant = min(chg1218_tct_vacant),
            max_chg1218_tct_vacant = max(chg1218_tct_vacant),
            sd_chg1218_tct_vacant = sd(chg1218_tct_vacant),
            
            mean_tct_newbuild18 = mean(tct_newbuild18),
            min_tct_newbuild18 = min(tct_newbuild18),
            max_tct_newbuild18 = max(tct_newbuild18),
            sd_tct_newbuild18 = sd(tct_newbuild18),
            
            mean_chg1218_tct_singfam = mean(chg1218_tct_singfam),
            min_chg1218_tct_singfam = min(chg1218_tct_singfam),
            max_chg1218_tct_singfam = max(chg1218_tct_singfam),
            sd_chg1218_tct_singfam = sd(chg1218_tct_singfam),
            
            mean_chg1218_tct_medhome_pct = mean(chg1218_tct_medhome_pct),
            min_chg1218_tct_medhome_pct = min(chg1218_tct_medhome_pct),
            max_chg1218_tct_medhome_pct = max(chg1218_tct_medhome_pct),
            sd_chg1218_tct_medhome_pct = sd(chg1218_tct_medhome_pct),
            
            mean_chg1218_tct_renters = mean(chg1218_tct_renters),
            min_chg1218_tct_renters = min(chg1218_tct_renters),
            max_chg1218_tct_renters = max(chg1218_tct_renters),
            sd_chg1218_tct_renters = sd(chg1218_tct_renters),
            
            mean_chg1218_tct_medrent_pct = mean(chg1218_tct_medrent_pct),
            min_chg1218_tct_medrent_pct = min(chg1218_tct_medrent_pct),
            max_chg1218_tct_medrent_pct = max(chg1218_tct_medrent_pct),
            sd_chg1218_tct_medrent_pct = sd(chg1218_tct_medrent_pct),
            
            mean_chg1218_tct_housdens = mean(chg1218_tct_housdens),
            min_chg1218_tct_housdens = min(chg1218_tct_housdens),
            max_chg1218_tct_housdens = max(chg1218_tct_housdens),
            sd_chg1218_tct_housdens = sd(chg1218_tct_housdens),
            
            mean_chg1218_tct_rentburd = mean(chg1218_tct_rentburd),
            min_chg1218_tct_rentburd = min(chg1218_tct_rentburd),
            max_chg1218_tct_rentburd = max(chg1218_tct_rentburd),
            sd_chg1218_tct_rentburd = sd(chg1218_tct_rentburd),
            
            mean_chg1218_tct_diffhou = mean(chg1218_tct_diffhou),
            min_chg1218_tct_diffhou = min(chg1218_tct_diffhou),
            max_chg1218_tct_diffhou = max(chg1218_tct_diffhou),
            sd_chg1218_tct_diffhou = sd(chg1218_tct_diffhou),
            
            mean_chg1218_tct_transit = mean(chg1218_tct_transit),
            min_chg1218_tct_transit = min(chg1218_tct_transit),
            max_chg1218_tct_transit = max(chg1218_tct_transit),
            sd_chg1218_tct_transit = sd(chg1218_tct_transit),
            
            mean_chg1218_tct_unemp = mean(chg1218_tct_unemp),
            min_chg1218_tct_unemp = min(chg1218_tct_unemp),
            max_chg1218_tct_unemp = max(chg1218_tct_unemp),
            sd_chg1218_tct_unemp = sd(chg1218_tct_unemp),
            
            mean_chg1218_tct_inpov = mean(chg1218_tct_inpov),
            min_chg1218_tct_inpov = min(chg1218_tct_inpov),
            max_chg1218_tct_inpov = max(chg1218_tct_inpov),
            sd_chg1218_tct_inpov = sd(chg1218_tct_inpov),
            
            mean_chg1218_tct_hhinc_pct = mean(chg1218_tct_hhinc_pct),
            min_chg1218_tct_hhinc_pct = min(chg1218_tct_hhinc_pct),
            max_chg1218_tct_hhinc_pct = max(chg1218_tct_hhinc_pct),
            sd_chg1218_tct_hhinc_pct = sd(chg1218_tct_hhinc_pct),
            
            mean_chg1218_tct_nonfam = mean(chg1218_tct_nonfam),
            min_chg1218_tct_nonfam = min(chg1218_tct_nonfam),
            max_chg1218_tct_nonfam = max(chg1218_tct_nonfam),
            sd_chg1218_tct_nonfam = sd(chg1218_tct_nonfam),
            
            mean_chg1218_tct_withba = mean(chg1218_tct_withba),
            min_chg1218_tct_withba = min(chg1218_tct_withba),
            max_chg1218_tct_withba = max(chg1218_tct_withba),
            sd_chg1218_tct_withba = sd(chg1218_tct_withba),
            
            mean_chg1218_tct_nonhispwh = mean(chg1218_tct_nonhispwh),
            min_chg1218_tct_nonhispwh = min(chg1218_tct_nonhispwh),
            max_chg1218_tct_nonhispwh = max(chg1218_tct_nonhispwh),
            sd_chg1218_tct_nonhispwh = sd(chg1218_tct_nonhispwh),
            
            mean_chg1218_tct_popgrowth = mean(chg1218_tct_popgrowth),
            min_chg1218_tct_popgrowth = min(chg1218_tct_popgrowth),
            max_chg1218_tct_popgrowth = max(chg1218_tct_popgrowth),
            sd_chg1218_tct_popgrowth = sd(chg1218_tct_popgrowth)
            
  ) %>% ungroup()

out <- out %>% pivot_longer(cols = 2:74)

write.csv(out, "./docs/gentri/descriptives_model.csv")