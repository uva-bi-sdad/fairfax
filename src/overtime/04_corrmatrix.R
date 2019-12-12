library(ggplot2)
library(GGally)
library(purrr)


# Create DFs for correlation matrix plots
dflist <- list(acs1317, acs1216, acs1115, acs1014, acs0913, acs0812, acs0711, acs0610, acs0509, dec2000, dec1990)
dflist <- map(dflist, ~ select(., hispanic, black, inpoverty, singleparent, lesshs) %>% st_set_geometry(NULL))
dflist <- set_names(dflist, c("ACS 2013-17", "ACS 2012-16", "ACS 2011-15", "ACS 2010-14", "ACS 2009-13", "ACS 2008-12", 
                              "ACS 2007-11", "ACS 2006-10", "ACS 2005-09", "Decennial 2000", "Decennial 1990"))

# Plot correlation
scattermat <- function(x, y) {
  ggpairs(x,
         upper = list(continuous = wrap("cor", size = 6, color = "black")),
         lower = list(continuous = wrap("smooth", color = "#00AFBB", size = 0.5)),
         diag = list(continuous = wrap("barDiag", fill = "#DCE319")),
         columnLabels = c("Hispanic", "Black", "In poverty", "Single parent", "Less than HS")) + 
    theme(plot.title = element_text(size = 16, face = "bold")) +
    labs(title = paste(y), subtitle = "Select data shown for tracts in Fairfax County, VA.")
}

corrplots <- map2(dflist, names(dflist), ~scattermat(.x, .y))

# Save plots
plotnames <- paste0(names(dflist), ".png")
plotnames

walk2(plotnames, corrplots, ~ggsave(filename = .x, plot = .y, height = 7, width = 7))
