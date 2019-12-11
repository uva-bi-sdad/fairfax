library(ggplot2)
library(GGally)


acs1317_corr <- acs1317 %>% select(hispanic, black, inpoverty, singleparent, lesshs) %>%
                            st_set_geometry(NULL)

ggpairs(acs1317_corr,
        upper = list(continuous = wrap("cor", size = 6, color = "black")),
        lower = list(continuous = wrap("smooth", color = "#00AFBB", size = 0.5)),
        diag = list(continuous = wrap("barDiag", fill = "#DCE319")),
        columnLabels = c("Hispanic", "Black", "In poverty", "Single parent", "Less than HS")) + 
  theme(plot.title = element_text(size = 16, face = "bold")) +
  labs(title = "ACS 2013-17", subtitle = "Select data shown for tracts in Fairfax County, VA.")



dflist <- list(acs1317, acs1216, acs1115, acs1014, acs0913, acs0812, acs0711, acs0610, acs0509)

newdfs <- dflist %>% map(~select(hispanic, black, inpoverty, singleparent, lesshs) %>% st_set_geometry(NULL))