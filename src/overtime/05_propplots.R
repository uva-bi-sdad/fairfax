library(ggthemes)
library(ggplot)
library(purrr)
library(dplyr)


#
# Prepare data and names ---------------------------------------------------------------------------------------------------------------------
#

dflist <- list(acs1317, acs1216, acs1115, acs1014, acs0913, acs0812, acs0711, acs0610, acs0509, dec2000, dec1990)
dflist <- set_names(dflist, c("ACS 2013-17", "ACS 2012-16", "ACS 2011-15", "ACS 2010-14", "ACS 2009-13", "ACS 2008-12", 
                              "ACS 2007-11", "ACS 2006-10", "ACS 2005-09", "Decennial 2000", "Decennial 1990"))

yrnames <- list("acs-2013-17", "acs-2012-16", "acs-2011-15", "acs-2010-14", "acs-2009-13", "acs-2008-12", 
                "acs-2007-11", "acs-2006-10", "acs-2005-09", "dec-2000", "dec-1990")


#
# Plot proportion Hispanic ---------------------------------------------------------------------------------------------------------------------
#

# Get min & max for scale
max_hisp <- sapply(dflist, function(x) max(x$hispanic, na.rm = TRUE)) %>% max()
min_hisp <- sapply(dflist, function(x) min(x$hispanic, na.rm = TRUE)) %>% min()

# Function
propplot_hisp <- function(x, y) {
  ggplot(x) +
    geom_sf(aes(fill = hispanic), size = 0.001) +
    labs(title = paste("Proportion Hispanic by tract,", y, sep = " ")) +
    theme_map() + 
    theme(plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 11, face = "bold"),
          legend.text = element_text(size = 11))  +
    scale_fill_viridis_c(name = "Proportion", limits = c(min_hisp, max_hisp), 
                         breaks = seq(round(min_hisp, 2), round(max_hisp, 2), round((min_hisp+max_hisp)/4, 2)))
}

# Plot
plots_hisp <- map2(dflist, dfnames, ~propplot_hisp(.x, .y))

for (i in 1:length(plots_hisp)) {
  assign(paste("plot_hisp", i, sep = "_"), plots_hisp[[i]])
}

# Save
plotnames_hisp <- paste0("hisp_", yrnames, ".png")
plotnames_hisp

walk2(plotnames_hisp, plots_hisp, ~ggsave(path = "./docs/varplots/", device = "png", filename = .x, plot = .y, height = 7, width = 7))


#
# Plot proportion Black ---------------------------------------------------------------------------------------------------------------------
#

# Get min & max for scale
max_black <- sapply(dflist, function(x) max(x$black, na.rm = TRUE)) %>% max()
min_black <- sapply(dflist, function(x) min(x$black, na.rm = TRUE)) %>% min()

# Function
propplot_black <- function(x, y) {
  ggplot(x) +
    geom_sf(aes(fill = black), size = 0.001) +
    labs(title = paste("Proportion Black by tract,", y, sep = " ")) +
    theme_map() + 
    theme(plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 11, face = "bold"),
          legend.text = element_text(size = 11))  +
    scale_fill_viridis_c(name = "Proportion", limits = c(min_black, max_black), 
                         breaks = seq(round(min_black, 2), round(max_black, 2), round((min_black+max_black)/4, 2)))
}

# Plot
plots_black <- map2(dflist, dfnames, ~propplot_black(.x, .y))

for (i in 1:length(plots_black)) {
  assign(paste("plot_black", i, sep = "_"), plots_black[[i]])
}

# Save
plotnames_black <- paste0("black_", yrnames, ".png")
plotnames_black

walk2(plotnames_black, plots_black, ~ggsave(path = "./docs/varplots/", device = "png", filename = .x, plot = .y, height = 7, width = 7))


#
# Plot proportion in poverty ---------------------------------------------------------------------------------------------------------------------
#

# Get min & max for scale
max_inpov <- sapply(dflist, function(x) max(x$inpoverty, na.rm = TRUE)) %>% max()
min_inpov <- sapply(dflist, function(x) min(x$inpoverty, na.rm = TRUE)) %>% min()

# Function
propplot_inpov <- function(x, y) {
  ggplot(x) +
    geom_sf(aes(fill = inpoverty), size = 0.001) +
    labs(title = paste("Proportion in poverty by tract,", y, sep = " ")) +
    theme_map() + 
    theme(plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 11, face = "bold"),
          legend.text = element_text(size = 11))  +
    scale_fill_viridis_c(name = "Proportion", limits = c(min_inpov, max_inpov), 
                         breaks = seq(round(min_inpov, 2), round(max_inpov, 2), round((min_inpov+max_inpov)/4, 2)))
}

# Plot
plots_inpov <- map2(dflist, dfnames, ~propplot_inpov(.x, .y))

for (i in 1:length(plots_inpov)) {
  assign(paste("plot_inpov", i, sep = "_"), plots_inpov[[i]])
}

# Save
plotnames_inpov <- paste0("inpov_", yrnames, ".png")
plotnames_inpov

walk2(plotnames_inpov, plots_inpov, ~ggsave(path = "./docs/varplots/", device = "png", filename = .x, plot = .y, height = 7, width = 7))


#
# Plot proportion less than high school ---------------------------------------------------------------------------------------------------------------------
#

# Get min & max for scale
max_lesshs <- sapply(dflist, function(x) max(x$lesshs, na.rm = TRUE)) %>% max()
min_lesshs <- sapply(dflist, function(x) min(x$lesshs, na.rm = TRUE)) %>% min()

# Function
propplot_lesshs <- function(x, y) {
  ggplot(x) +
    geom_sf(aes(fill = lesshs), size = 0.001) +
    labs(title = paste("Proportion with <HS education by tract,", y, sep = " ")) +
    theme_map() + 
    theme(plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 11, face = "bold"),
          legend.text = element_text(size = 11))  +
    scale_fill_viridis_c(name = "Proportion", limits = c(min_lesshs, max_lesshs), 
                         breaks = seq(round(min_lesshs, 2), round(max_lesshs, 2), round((min_lesshs+max_lesshs)/4, 2)))
}

# Plot
plots_lesshs <- map2(dflist, dfnames, ~propplot_lesshs(.x, .y))

for (i in 1:length(plots_lesshs)) {
  assign(paste("plot_lesshs", i, sep = "_"), plots_lesshs[[i]])
}

# Save
plotnames_lesshs <- paste0("lesshs_", yrnames, ".png")
plotnames_lesshs

walk2(plotnames_lesshs, plots_lesshs, ~ggsave(path = "./docs/varplots/", device = "png", filename = .x, plot = .y, height = 7, width = 7))


#
# Plot proportion single parent ---------------------------------------------------------------------------------------------------------------------
#

# Get min & max for scale
max_single <- sapply(dflist, function(x) max(x$singleparent, na.rm = TRUE)) %>% max()
min_single <- sapply(dflist, function(x) min(x$singleparent, na.rm = TRUE)) %>% min()

# Function
propplot_single <- function(x, y) {
  ggplot(x) +
    geom_sf(aes(fill = singleparent), size = 0.001) +
    labs(title = paste("Proportion single parent families by tract,", y, sep = " ")) +
    theme_map() + 
    theme(plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 11, face = "bold"),
          legend.text = element_text(size = 11))  +
    scale_fill_viridis_c(name = "Proportion", limits = c(min_single, max_single), 
                         breaks = seq(round(min_single, 2), round(max_single, 2), round((min_single+max_single)/4, 2)))
}

# Plot
plots_single <- map2(dflist, dfnames, ~propplot_single(.x, .y))

for (i in 1:length(plots_single)) {
  assign(paste("plot_single", i, sep = "_"), plots_single[[i]])
}

# Save
plotnames_single <- paste0("singleparent_", yrnames, ".png")
plotnames_single

walk2(plotnames_single, plots_single, ~ggsave(path = "./docs/varplots/", device = "png", filename = .x, plot = .y, height = 7, width = 7))
