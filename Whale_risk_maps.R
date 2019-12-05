# Plot heat maps of simple linear risk from 'Whale_risk_timeseries.R'

# SMW todo

###############################################################################
library(dplyr)
library(maps)
library(RColorBrewer)
library(sf)

source("plot_raimbow.R")


###############################################################################
source("User_script_local.R")
if (user == "JS") {
  
} else if (user == "SMW") {
  file.data.grid <- "../raimbow-local/Data/5x5 km grid shapefile/five_km_grid_polys_geo.shp"
  path.rdata <- "../raimbow-local/RDATA_files/"
  path.plots <- "../raimbow-local/Plots/Whale_risk_maps/"
  
} else {
  stop("User not recognized")
}




###############################################################################
load(paste0(path.rdata, "Whale_risk_formaps.Rdata"))
# d <- sapply(fish.out, which.max)
# table(fish.out$GRID5KM_ID[unname(unlist(d))])

map.base <- st_geometry(st_as_sf(maps::map('world', plot = FALSE, fill = TRUE)))
load(paste0(path.rdata, "Grid_5km_landerased.RDATA"))

h.sf <- humpback.all %>% 
  left_join(grid.5km.lno, by = "GRID5KM_ID") %>% 
  st_sf(agr = "constant") %>% 
  select(starts_with("Mn_"))

f.sf <- fish.out %>% 
  left_join(grid.5km.lno, by = "GRID5KM_ID") %>% 
  st_sf(agr = "constant") %>% 
  select(starts_with("DC_"))

r.sf <- risk.all %>% 
  left_join(grid.5km.lno, by = "GRID5KM_ID") %>% 
  st_sf(agr = "constant") %>% 
  select(starts_with("Mn_DC_risk"))

# summary(unlist(st_drop_geometry(h.sf)))
# summary(unlist(st_drop_geometry(f.sf)))
# summary(unlist(st_drop_geometry(r.sf)))
# head(sort(unlist(st_drop_geometry(f.sf)), decreasing = TRUE), 20)

h.br <- seq(0, max(st_drop_geometry(h.sf), na.rm = TRUE), length.out = 7)
f.br <- ceiling(seq(0, max(st_drop_geometry(f.sf), na.rm = TRUE), length.out = 7))
r.br <- ceiling(seq(0, max(st_drop_geometry(r.sf), na.rm = TRUE), length.out = 7))

col.pal <- rev(brewer.pal(6, "YlGnBu"))


###############################################################################




###############################################################################
# plot.list <- list(xlim = c(35, 45), asp = 0)

layout(matrix(1:3, nrow = 1))
plot_raimbow(
  h.sf, "Mn_2009_11", grid.5km.lno, map.base, 
  col.pal = col.pal, col.breaks = h.br, 
  asp = 0, ylim = c(35, 45)
)
legend_raimbow(h.br, "%0.2f", fill = rev(col.pal), cex = 1.4)

plot_raimbow(
  f.sf, "DC_2009_11", NULL, map.base, 
  col.pal = col.pal, col.breaks = f.br, 
  asp = 0, ylim = c(35, 45)
)
legend_raimbow(f.br, NULL, fill = rev(col.pal), cex = 1.4)

plot_raimbow(
  r.sf, "Mn_DC_risk_2009_11", NULL, map.base, 
  col.pal = col.pal, col.breaks = r.br, 
  asp = 0, ylim = c(35, 45)
)
legend_raimbow(r.br, NULL, fill = rev(col.pal), cex = 1.4)


###############################################################################
key.txt <- apply(df.key.ym, 1, paste, collapse = "_")

for (i in key.txt) {
  h.curr <- paste0("Mn_", i)
  f.curr <- paste0("DC_", i)
  r.curr <- paste0("Mn_DC_risk_", i)
  
  png(paste0(path.plots, i, ".png"), height = 4, width = 7, units = "in", res = 300)
  
  layout(matrix(1:3, nrow = 1))
  plot_raimbow(
    h.sf, h.curr, grid.5km.lno, map.base, 
    col.pal = col.pal, col.breaks = h.br, 
    asp = 0, ylim = c(34, 48), xaxt = "n", 
    main = paste("Humpback abund", i)
  )
  legend_raimbow(h.br, "%0.2f", fill = rev(col.pal), cex = 1.2)
  
  plot_raimbow(
    f.sf, f.curr, NULL, map.base, 
    col.pal = col.pal, col.breaks = f.br, 
    asp = 0, ylim = c(34, 48), xaxt = "n", 
    main = paste("Non-conf VMS pings", i)  )
  legend_raimbow(f.br, NULL, fill = rev(col.pal), cex = 1.2)
  
  plot_raimbow(
    r.sf, r.curr, NULL, map.base, 
    col.pal = col.pal, col.breaks = r.br, 
    asp = 0, ylim = c(34, 48), xaxt = "n", 
    main = paste("Risk (linear)", i)  )
  legend_raimbow(r.br, NULL, fill = rev(col.pal), cex = 1.2)
  
  dev.off()
}



###############################################################################
