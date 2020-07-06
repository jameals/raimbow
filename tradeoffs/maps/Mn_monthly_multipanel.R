### Make multipanel plot of monthly Mn 3km preds
# https://github.com/karinforney/Mn3kmSDM/blob/master/Mn_monthly_multipanel.R

###############################################################################
library(dplyr)
library(eSDM)
library(RColorBrewer)
library(rnaturalearth)
library(sf)

### Requires tmap 3.1, version not on CRAN yet
# remotes::install_github("mtennekes/tmaptools")
# remotes::install_github("mtennekes/tmap")
library(tmap)

source("User_script_local.R")
if (user == "KAF") {
  
  
} else if (user == "SMW") {
  path.mn.monthly <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/Mn_3km_monthly.RDS"
  path.mn.monthly.sf <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Data/Mn_3km_monthly_sf.RDS"
  
  file.out.all.pre <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_monthly_multiplanel_"
  file.out.sub <- "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_monthly_multiplanel_sub.png"
}


###############################################################################
### Create sf object
# x <- readRDS(path.mn.monthly)
# 
# x.geom <- x %>% 
#   select(mlon, mlat, pixel) %>% 
#   distinct() %>% 
#   pts2poly_centroids(0.027, crs = 4326, agr = "constant")
# 
# x.sf <- x %>% #~2 minutes
#   mutate(month_fac = lubridate::month(month, label = TRUE, abbr = FALSE)) %>% 
#   left_join(x.geom, by = "pixel") %>% 
#   st_as_sf()
# 
# saveRDS(x.sf, path.mn.monthly.sf)
x.sf <- readRDS(path.mn.monthly.sf)


###############################################################################
# Prep for multipanel, and plot

#------------------------------------------------------------------------------
### Prep

# Color palette and break points
col.breaks <- c(0, 0.01, 0.02, 0.03, 0.05, 0.08)
col.pal <- brewer.pal(length(col.breaks) - 1, "Blues")
stopifnot((length(col.breaks) - 1) == length(col.pal))
#To see color palette options, run: RColorBrewer::display.brewer.all()
#Also see: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html#

# Plot limits
x.bbox <- st_bbox(x.sf)

plot.xmin <- x.bbox[["xmin"]] - 1
plot.xmax <- x.bbox[["xmax"]]
plot.ymin <- x.bbox[["ymin"]]
plot.ymax <- x.bbox[["ymax"]]

x.asp.ratio <- unname(
  (x.bbox["xmax"] - x.bbox["xmin"]) / (x.bbox["ymax"] - x.bbox["ymin"])
)

# Base geometry
# r1 <- st_geometry(ne_states(country = "United States of America", returnclass = "sf"))
# r2 <- st_geometry(ne_countries(scale = 50, continent = "North America", returnclass = "sf"))
# rmap.base <- c(r1, r2)

rmap.base <- c(
  st_geometry(ne_states(country = "United States of America", returnclass = "sf")), 
  st_geometry(ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>% 
                filter(admin %in% c("Canada", "Mexico")))
) %>% 
  st_crop(xmin = plot.xmin - 1, xmax = plot.xmax + 1, ymin = plot.ymin - 1, ymax = plot.ymax + 1)

# rmap.base <- st_geometry(ne_states(country = "United States of America", returnclass = "sf")) %>% 
#   st_geometry() %>% 
#   st_crop(xmin = plot.xmin - 1, xmax = plot.xmax + 1, ymin = plot.ymin - 1, ymax = plot.ymax + 1)
# # Can add names, state boundaries, etc.
# rmap.base2 <- ne_countries(scale = 10, continent = "North America", returnclass = "sf") %>% 
#   filter(admin %in% c("Canada", "Mexico")) %>% 
#   st_geometry()


# Just in case
tmap_mode("plot")


#------------------------------------------------------------------------------
### Plot subset -  taks ~30 minutes total to run
x.toplot.sub <- x.sf %>%
  filter(.data$month %in% c(1, 4, 7, 10),
         # .data$pixel %in% sample(unique(x.sf$pixel), 1000),
         .data$year %in% c(2012:2017))

tm.obj.sub <- tm_shape(rmap.base, bbox = x.bbox) +
  tm_polygons() +
  tm_shape(x.toplot.sub) +
  tm_fill(col = "mn_avgdens", border.col = "transparent",
          style = "fixed", breaks = col.breaks, palette = col.pal,
          colorNA = NULL, showNA = FALSE,
          title = "Whales / km2", legend.reverse = TRUE) +
  tm_facets(by = c("year", "month_fac")) +
  tm_legend(outside = TRUE, position = c("right", "center"), outside.size = 0.3,
            title.size = 1.8, text.size = 1) +
  # tm_graticules(ticks = TRUE, lines = FALSE, n.y = 5, n.x = 5, labels.size = 1) +
  tm_layout(panel.label.size = 2.3, inner.margins = 0) #, asp = x.asp.ratio)

# tmap_save(tm.obj.sub, filename = "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_monthly_multiplanel_sub_test.png",
#           asp = 0, units = "in", width = 8, height = 15) #width.in/x.asp.ratio)

tmap_save(tm.obj.sub, filename = file.out.sub, #"C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_monthly_multiplanel_sub_v2a.png", #
          width = 4.2, height = 9, units = "in", asp = 0)

# tmap_save(tm.obj.sub, filename = "C:/SMW/RAIMBOW/Mn3kmSDM/Ignore/Plots/Mn_monthly_multiplanel_sub_v2b.png", #
#           width = 6, height = 9, units = "in", asp = 0)



#------------------------------------------------------------------------------
### Plot all of them - # takes ~15 minutes per year (i.e. per plot)
for (i in 2005:2018) {
  print(paste(i, Sys.time(), sep = " - "))
  x.toplot.curr <- x.sf %>% filter(year == i) #%>% filter(pixel %in% sample(unique(x.sf$pixel), 3000))
  
  tm.obj.all <- tm_shape(rmap.base, bbox = x.bbox) +
    tm_polygons() +
    tm_shape(x.toplot.curr) +
    tm_fill(col = "mn_avgdens", border.col = "transparent",
            style = "fixed", breaks = col.breaks, palette = col.pal,
            colorNA = NULL, showNA = FALSE,
            title = "Whales / km2", legend.reverse = TRUE) +
    tm_facets(by = c("month_fac"), nrow = 3, ncol = 4,
              free.coords = FALSE, free.scales = FALSE) +
    tm_legend(outside = TRUE, position = c("right", "center"), outside.size = 0.25,
              title.size = 1.8, text.size = 1) +
    # tm_graticules(ticks = TRUE, lines = FALSE, n.y = 5, n.x = 5, labels.size = 1) +
    tm_layout(panel.label.size = 1)
  
  tmap_save(tm.obj.all, filename = paste0(file.out.all.pre, i, ".png"),
            width = 5.6, height = 7, units = "in", asp = 0)
}

###############################################################################