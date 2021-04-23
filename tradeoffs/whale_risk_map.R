library(tidyverse)
library(sf)


file.contour <- "C:/SMW/RAIMBOW/raimbow-local/Data/West_coast_bathy/West_Coast_geo.shp"
x.df <- read.csv("../raimbow-local/Data/Data frame for Figure 1 Samhouri et al.csv") 
path.plots <- ("../raimbow-local/Plots/")

source('C:/SMW/RAIMBOW/raimbow/humpback_risk/plot_raimbow.R')


# Prep - background map objects
map.contours <- st_read(file.contour)
map.contours.050m <- map.contours %>% filter(Contour == -50) %>% st_geometry()
map.contours.100m <- map.contours %>% filter(Contour == -100) %>% st_geometry()
map.contours.200m <- map.contours %>% filter(Contour == -200) %>% st_geometry()

map.base <- st_geometry(st_as_sf(maps::map('world', plot = FALSE, fill = TRUE)))
grid.5km.lno <- readRDS("C:/SMW/RAIMBOW/raimbow-local/RDATA_files/Grid_5km_landerased.rds")



summary(x.df$Num_DCRB_VMS_pings)
summary(x.df$Humpback_dens)
summary(x.df$Blue_occurrence)

q.probs <- c(0, 0.25, 0.50, 0.75, 1)
# quantile(x.df$Num_DCRB_VMS_pings, q.probs)
b.hump <- c(0, as.numeric(quantile(x.df$Humpback_dens, q.probs, na.rm = TRUE)))
b.blue <- c(0, as.numeric(quantile(x.df$Blue_occurrence, q.probs, na.rm = TRUE)))
b.pings <- c(0, 0.05, 0.1, 2, 20, max(x.df$Num_DCRB_VMS_pings, na.rm = TRUE))

col.pal <- rev(RColorBrewer::brewer.pal(length(q.probs) + 1, "YlGnBu"))


x.sf <- x.df %>% 
  # st_sf(crs = 4326, sf_column_name = geometry)
  select(-geometry, -area_km_lno) %>% 
  left_join(grid.5km.lno, by = "GRID5KM_ID") %>% 
  st_as_sf()



for (time.period in unique(x.df$time_period)) {
  print(time.period)
  # time.period <- "2009-2014"
  x.sf.curr <- x.sf %>% filter(time_period == time.period)
  
  png(paste0(path.plots, "summ_", time.period, ".png"), height = 4, width = 7, units = "in", res = 300)
  layout(matrix(1:3, nrow = 1))
  
  plot_raimbow(
    x.sf.curr, "Blue_occurrence", NULL, map.base, 
    map.i050 = map.contours.050m, map.i100 = map.contours.100m, map.i200 = map.contours.200m, 
    col.pal = col.pal, col.breaks = b.blue, 
    asp = 0, ylim = c(34, 43), xaxt = "n", 
    main = paste("Blue prob of occ -", time.period)
  )
  legend_raimbow(b.blue, "%0.2f", fill = rev(col.pal), cex = 1.2, title = "Blue whale PoO")
  legend.raimbow.bathy()
  
  
  plot_raimbow(
    x.sf.curr, "Humpback_dens", NULL, map.base, 
    map.i050 = map.contours.050m, map.i100 = map.contours.100m, map.i200 = map.contours.200m, 
    col.pal = col.pal, col.breaks = b.hump, 
    asp = 0, ylim = c(34, 43), xaxt = "n", 
    main = paste("Humpback density -", time.period)
  )
  legend_raimbow(b.hump, "%0.2f", fill = rev(col.pal), cex = 1.2, title = "Humpback density")
  legend.raimbow.bathy()
  
  
  plot_raimbow(
    x.sf.curr, "Num_DCRB_VMS_pings", NULL, map.base, 
    map.i050 = map.contours.050m, map.i100 = map.contours.100m, map.i200 = map.contours.200m, 
    col.pal = col.pal, col.breaks = b.pings, 
    asp = 0, ylim = c(34, 43), xaxt = "n", 
    main = paste("VMS pings -", time.period)
  )
  legend_raimbow(b.pings, "%0.2f", fill = rev(col.pal), cex = 1.2, title = "Pings")
  legend.raimbow.bathy()
  
  dev.off()
}
